"""
direc/*.c direc/*.cc direc/*.cpp direc/*.cxx direc/*.c++
direc/*.h direc/*.hh direc/*.hpp direc/*.hxx direc/*.h++
"""

from collections import deque
from collections.abc import Iterator, Iterable
from dataclasses import dataclass, make_dataclass
from itertools import chain, takewhile
import typing as ty

def Symbol(name: str):
  name = f":{name}"

  def _no_copy(*_):
    raise TypeError(f"'{name}' object is not copyable")

  cls = make_dataclass(name, (), frozen=True,
    namespace={
      "__repr__": lambda _: name,
      "__copy__": _no_copy,
      "__deepcopy__": _no_copy,
    })
  return cls()

EndOfStatement = Symbol("EndOfStatement")


@dataclass(frozen=True)
class WordToken:
  word: str
  def __str__(self):
    return self.word

  def __repr__(self):
    return f"W:{self.word}"

Token = ty.Union[str, WordToken] # str represents all special tokens

def is_a_word(token: Token):
  return isinstance(token, WordToken)


Expr = ...

@dataclass(frozen=True)
class ParenEx:
  '''
  A tuple of expressions representing a parenthesized expression, including
  the opening and the closing parentheses.
  '''
  tokens: tuple[Expr]


@dataclass(frozen=True)
class TemplEx:
  '''
  A tuple of expressions representing a template expression,
  which includes 1 token before the < and everything until & including the closing >.
  '''
  tokens: tuple[Expr]


Expr = ty.Union[Token, TemplEx, ParenEx]

def is_a_paren_ex(expr: Expr):
  return isinstance(expr, ParenEx)

def is_a_templ_ex(expr: Expr):
  return isinstance(expr, TemplEx)



class TextReader:
  def __init__(self, stream):
    self._stream = stream
    self._buf = deque()
    self._newline_count = 0

  def line_count(self):
    return self._newline_count + 1

  def __next__(self):
    if len(self._buf) > 0:
      if self._buf[0] == "\n":
        self._newline_count += 1
      return self._buf.popleft()
    buf = self._stream.read(256)
    if buf != "":
      self._buf.extend(buf[1:])
      if buf[0] == "\n":
        self._newline_count += 1
      return buf[0]
    raise StopIteration

  def __iter__(self):
    return self

  def peek(self, idx):
    if idx < len(self._buf):
      return self._buf[idx]

    self._buf.extend(self._stream.read(idx - len(self._buf) + 1))

    if idx < len(self._buf):
      return self._buf[idx]
    else:
      return ""

  def discard(self):
    if len(self._buf) > 0:
      ch = self._buf.popleft()
    else:
      ch = self._stream.read(1)
    if ch == "\n":
      self._newline_count += 1
    return ch


def tokenize(stream: TextReader) -> Iterator[Token]:
  def one_char():
    yield ch

  def string_lit_state():
    for ch in stream:
      if ch == "\"":
        yield "\"\""
        break
      elif ch == "\\":
        stream.discard()

  def char_lit_state():
    char_token = ""
    for ch in stream:
      if ch == "'":
        yield f"'{char_token}'"
        break
      elif ch == "\\":
        for ch in stream:
          char_token += "\\" + ch
          break
      else:
        char_token += ch

  def slash_state():
    ch = stream.peek(0)
    if ch == "*": # multiline comment
      stream.discard()
      for ch in stream:
        if ch == "*" and stream.peek(0) == "/":
          stream.discard()
          break
    elif ch == "/": # inline comment
      stream.discard()
      for ch in stream:
        if ch == "\n":
          yield "\n" # must always replace inline comments with a newline
          break
    elif ch == "=":
      stream.discard()
      yield "/="
    else:
      yield "/"

  def one_or_two_chars(first_char, second_char_list):
    def _impl():
      ch = stream.peek(0)
      if ch == "":
        yield first_char
      elif ch in second_char_list:
        stream.discard()
        yield first_char + ch
      else:
        yield first_char
    return _impl

  def angle_state(angle_char):
    def _impl():
      ch = stream.peek(0)
      if ch == angle_char:
        stream.discard()
        if stream.peek(0) == "=":
          stream.discard()
          yield f"{angle_char}{angle_char}=" # shift-assignment operator
        else:
          yield f"{angle_char}{angle_char}" # shift operator
      elif ch == "=":
        stream.discard()
        yield f"{angle_char}=" # greater than/less than or equal
      else:
        yield angle_char
    return _impl

  def dot_sequence_state():
    ch = stream.peek(0)
    if ch == ".":
      ch = stream.peek(1)
      if ch == ".":
        stream.discard()
        stream.discard()
        yield "..."
      else:
        yield "." # two consecutive dots aren't valid in c++
    else:
      yield "."

  def space_state():
    ch = stream.peek(0)
    while ch != "" and ch in " \t\r\f":
      stream.discard()
      ch = stream.peek(0)
    return ()

  special_states = {
    " ": space_state,
    "\r": space_state,
    "\t": space_state,
    "\f": space_state,
    "{": one_char,
    "}": one_char,
    "(": one_char,
    ")": one_char,
    "[": one_char,
    "]": one_char,
    ";": one_char,
    ",": one_char,
    "#": one_char,
    "?": one_char,
    "\\": one_char,
    "\n": one_char,
    "'": char_lit_state,
    "\"": string_lit_state,
    ".": dot_sequence_state,
    "/": slash_state,
    ">": angle_state(">"),
    "<": angle_state("<"),
    "!": one_or_two_chars("!", "="),
    "^": one_or_two_chars("^", "="),
    "+": one_or_two_chars("+", "+="),
    "-": one_or_two_chars("-", "->="),
    "=": one_or_two_chars("=", "="),
    ":": one_or_two_chars(":", ":"),
    "&": one_or_two_chars("&", "&="),
    "|": one_or_two_chars("|", "|="),
    "*": one_or_two_chars("*", "/="),
    "%": one_or_two_chars("%", "="),
  }

  word = ""

  for ch in stream:
    special_state = special_states.get(ch, None)
    if special_state is None:
      word += ch

    elif word == "":
      yield from special_state()

    else:
      yield WordToken(word)
      yield from special_state()
      word = ""

  if word != "":
    yield WordToken(word)


# TODO: see if can put no_preproc directly into tokenize
def no_preproc(tokens: Iterator[Token]) -> Iterator[Token]:
  def preproc_state():
    for t in tokens:
      if t == "\n":
        return
      elif t == "\\":
        for t in tokens:
          break

  for t in tokens:
    if t == "#":
      preproc_state()
    elif t != "\n":
      yield t


# TODO: see if can put join_tokens directly into make_expressions
def join_tokens(tokens: Iterator[Token]) -> Iterator[Expr]:
  numeric_suffixes = frozenset(("f", "l", "F", "L",
    "f16", "f32", "f64", "f128", "bf16","F16", "F32", "F64", "F128", "BF16"))

  def is_lead_by_a_digit(word: str):
    ch = ord(word[0])
    return ord("0") <= ch <= ord("9")

  def dispatch():
    if is_a_word(t) and t.word == "operator":
      back_buf.append(t)
      return operator_keyword
    elif is_a_word(t) and is_lead_by_a_digit(t.word):
      back_buf.append(t)
      return number_state
    elif t == ".":
      back_buf.append(t)
      return leading_dot
    else:
      buf.append(t)
      return dispatch

  def number_state():
    if t == ".":
      back_buf.append(".")
      return number_dot

    elif is_a_word(t) and is_lead_by_a_digit(t.word):
      buf.extend(back_buf)
      del back_buf[:]
      back_buf.append(t)
      return number_state

    else:
      buf.extend(back_buf)
      buf.append(t)
      del back_buf[:]
      return dispatch

  def leading_dot():
    if is_a_word(t) and is_lead_by_a_digit(t.word):
      buf.append(WordToken("." + t.word))
    else:
      buf.extend((".", t))

    del back_buf[:]
    return dispatch

  def number_dot():
    if is_a_word(t) and (is_lead_by_a_digit(t.word) or t.word in numeric_suffixes):
      buf.append(WordToken(back_buf[0].word + "." + t.word)) # x.y format
    else:
      buf.extend((WordToken(back_buf[0].word + "."), t)) # x. format

    del back_buf[:]
    return dispatch

  def operator_keyword():
    if is_a_word(t):
      buf.extend(back_buf)
      del back_buf[:]
      return dispatch
    elif t == "[":
      back_buf.append(t)
      return operator_sq_br
    elif t == "(":
      back_buf.append(t)
      return operator_paren
    elif t == "<=":
      back_buf.append(t)
      return operator_spaceship
    else:
      buf.append(WordToken("operator" + t))
      del back_buf[:]
      return dispatch

  def operator_sq_br():
    if t == "]":
      buf.append(WordToken("operator[]"))
    else:
      buf.extend((WordToken("operator"), "[", t))
    del back_buf[:]
    return dispatch

  def operator_paren():
    if t == ")":
      buf.append(WordToken("operator()"))
    else:
      buf.extend((WordToken("operator"), "(", t))
    del back_buf[:]
    return dispatch

  def operator_spaceship():
    if t == ">":
      buf.append(WordToken("operator<=>"))
    else:
      buf.extend((WordToken("operator<="), t))
    del back_buf[:]
    return dispatch

  back_buf = []
  buf = []
  state = dispatch
  for t in tokens:
    state = state()
    yield from buf
    del buf[:]

  yield from buf
  yield from back_buf


def make_expressions(tokens: Iterator[Token]) -> Iterator[Expr]:
  def dispatch(): # back_buf is empty
    if t == "(":
      back_buf.append(t)
      return paren_state(1)

    elif is_a_word(t):
      back_buf.append(t)
      return word_state

    else:
      buf.append(t)
      return dispatch

  def word_state(): # back_buf contains a single word token
    if t == "<":
      back_buf.append(t)
      return template_state(1)
    elif is_a_word(t):
      buf.extend(back_buf)
      del back_buf[:]
      back_buf.append(t)
      return word_state
    elif t == "(":
      buf.extend(back_buf)
      del back_buf[:]
      back_buf.append(t)
      return paren_state(1)
    else:
      buf.extend(back_buf)
      buf.append(t)
      del back_buf[:]
      return dispatch

  def paren_state(depth): # back_buf contains a '(' and 0 or more additional tokens 
    def _impl():
      back_buf.append(t)

      if t == "(":
        return paren_state(depth + 1)
      elif t == ")":
        if depth > 1:
          return paren_state(depth - 1)

        buf.append(ParenEx(tuple(back_buf)))
        del back_buf[:]
        return dispatch
      else:
        return _impl
    return _impl

  def template_state(depth): # back_buf contains a word token, a '<', and 0 or more additional tokens
    def _impl():
      back_buf.append(t)

      if t == "<":
        return template_state(depth + 1)
      elif t in (">", ">>"):
        if depth > len(t):
          return template_state(depth - len(t))

        buf.append(TemplEx(tuple(back_buf)))
        del back_buf[:]
        return dispatch
      else:
        return _impl
    return _impl

  back_buf = []
  buf = []
  state = dispatch
  for t in tokens:
    state = state()
    yield from buf
    del buf[:]

  yield from buf
  yield from back_buf



@dataclass(frozen=True)
class FunctionDecl:
  line_number: int
  name: WordToken
  namespace: tuple[Token]
  templ: ty.Optional[TemplEx]
  args: ParenEx
  rtype: tuple[Token]


@dataclass(frozen=True)
class Scope:
  kind: WordToken
  name: tuple[Token]


class ScopeStack(list[Scope]):
  def __init__(self):
    super().__init__()
    self.in_function_body = False

  def append(self, scope: Scope):
    super().append(scope)
    if scope.kind != "":
      self.in_function_body = scope.kind == "function"

  def pop(self) -> Scope:
    if len(self) == 0:
      return None

    ret = super().pop()
    for scope in reversed(self):
      if scope.kind != "":
        self.in_function_body = scope.kind == "function"
        return ret

    self.in_function_body = False
    return ret

  def iter_named_scopes(self) -> Iterator[Scope]:
    for scope in self:
      if scope.kind in named_scope_kinds:
        yield scope





def write_tokens(stream, tokens: Iterable[Token]):
  sep = ""

  def _unpack_complex_tokens():
    for t in tokens:
      if is_a_templ_ex(t):
        yield t.tokens[0].word + "<~~>"
      elif is_a_paren_ex(t):
        yield "(~~)"
      else:
        yield t

  for t in _unpack_complex_tokens():
    if is_a_word(t):
      stream.write(sep)
      stream.write(str(t))
      sep = " "
    elif t in (EndOfStatement, ".", "'", "::"):
      if t is EndOfStatement:
        stream.write("\n")
      else:
        stream.write(t)
      sep = ""
    elif t in (")", ","):
      stream.write(t)
      sep = " "
    else:
      stream.write(sep)
      stream.write(t)
      sep = "" if t in ("\\", "(") else " " # don't separate the backslash from the next char






named_scope_kinds = frozenset(("namespace", "struct", "class"))
def join_scopes(scopes: Iterable[Scope]) -> Iterator[Expr]:

  def _get_class_name(decl: Iterable[Expr]) -> Expr:
    clean_decl = tuple(takewhile(decl, lambda t: t != ":")) # stop at the inheritance list

    if len(clean_decl) == 0:
      return ""
    elif clean_decl[-1] == "final":
      return clean_decl[-2] if len(clean_decl) > 1 else ""
    else:
      return clean_decl[-1]

  scopes = iter(scopes)
  for scope in scopes:
    if scope.kind in named_scope_kinds:
      yield _get_class_name(scope.name)
    for scope in scopes:
      if scope.kind in named_scope_kinds:
        yield "::"
        yield _get_class_name(scope.name)
    break


def write_function_cdef(stream, func_def: FunctionDecl):
  if func_def.templ:
    write_tokens(stream, func_def.templ)
    stream.write("\n")

  write_tokens(stream, join_scopes(func_def.namespace))
  if func_def.namespace:
    stream.write(":: ")

  stream.write("cdef")

  if func_def.rtype:
    stream.write(f" {func_def.name}(")
    write_tokens(stream, func_def.args)
    stream.write("): ")
    write_tokens(stream, func_def.rtype)

  else:
    if func_def.name.word.startswith("~"):
      stream.write(f" destructor {func_def.name}(")
    else:
      stream.write(f" constructor {func_def.name}(")
    write_tokens(stream, func_def.args)
    stream.write(")")


def write_function_cpp2(stream, func_def: FunctionDecl):
  if func_def.templ:
    write_tokens(stream, func_def.templ)
    stream.write("\n")

  write_tokens(stream, join_scopes(func_def.namespace))
  if func_def.namespace:
    stream.write("::")

  stream.write(f"{func_def.name}: (")
  write_tokens(stream, func_def.args)
  if func_def.rtype:
    stream.write(") -> ")
    write_tokens(stream, func_def.rtype)
  else:
    stream.write(")")


def write_functions(stream, func_defs, filename: str, cpp2=False):
  for func_def in func_defs:
    stream.write(f"// {filename}:{func_def.line_number}\n")
    if cpp2:
      write_function_cpp2(stream, func_def)
    else:
      write_function_cdef(stream, func_def)

    stream.write(";\n\n")



def clean_func_decl(decl):
  for t in decl:
    if is_a_word(t):
      if t.word not in ("public", "private", "protected"):
        yield t
    elif t != ":":
      yield t


type_tokens = frozenset(("...", ",", "::", "*", "&", "&&", "[", "]", ":"))
def parse_statement(stmt, scope_stack: ScopeStack, line_ctx: TextReader):
  paren_preamble =[]

  for t in stmt:
    if is_a_word(t):
      if t.word in named_scope_kinds:
        paren_preamble = []
        sc_kind = t.word
        sc_preamble = []
        for t in stmt:
          if is_a_word(t) or is_a_templ_ex(t) or t in type_tokens:
            if is_a_templ_ex(t):
              sc_preamble.extend(t.tokens)
            else:
              sc_preamble.append(t)

          elif t == "{":
            scope_stack.append(Scope(sc_kind, tuple(sc_preamble)))
            break

          elif t == ";" and sc_kind != "namespace": # if forward declaration
            break

      else:
        paren_preamble.append(t)

    elif t == "(":
      if scope_stack.in_function_body:
        continue

      decl_line = line_ctx.line_count()
      func_decl = tuple(clean_func_decl(paren_preamble))
      paren_preamble = []

      if not func_decl:
        continue

      func_args = []
      depth = 1

      for t in stmt:
        if t == "(":
          depth += 1

        elif t == ")":
          depth -= 1
          if depth == 0:
            for t in stmt:
              if t == "{": # start of function body, unless we're in a ctor initializer list
                scope_stack.append(Scope("function", func_decl))
              elif t == "}":
                scope_stack.pop()

            func_templ, func_rtype, func_scope, func_name = split_func_decl(func_decl)
            func_scope = tuple(chain(scope_stack.iter_named_scopes(), func_scope))
            yield FunctionDecl(
              decl_line,
              func_name,
              func_scope,
              func_templ,
              tuple(func_args),
              func_rtype)
            break

        else:
          func_args.append(t)

    elif t == "{":
      scope_stack.append(Scope("", ()))

    elif t == "}":
      scope_stack.pop()

    elif t in type_tokens:
      paren_preamble.append(t)

    elif is_a_templ_ex(t):
      paren_preamble.extend(t.tokens)


def parse_functions(tokens, line_ctx: TextReader):
  has_more_tokens = [False]

  def _statement():
    has_more_tokens[0] = False
    for t in tokens:
      has_more_tokens[0] = True
      if t is EndOfStatement:
        break
      yield t

  scope_stack = ScopeStack()

  while True:
    stmt = join_template_expressions(_statement())
    yield from parse_statement(stmt, scope_stack, line_ctx)

    if not has_more_tokens[0]:
      break




def statement_split(tokens):
  def dispatch():
    if t == ";":
      back_buf.append(t)
      return semi
    else:
      buf.append(t)
      return dispatch

  def semi():
    if t == "}":
      back_buf.append(t)
      return semi_and_at_least_1_curly
    elif t == ";":
      buf.extend((";", EndOfStatement))
      return semi
    else:
      buf.extend((";", EndOfStatement, t))
      del back_buf[:]
      return dispatch

  def semi_and_at_least_1_curly():
    # back_buf now contains one semi followed by one or more curlies
    if t == "}":
      back_buf.append(t)
      return semi_and_at_least_1_curly

    elif t == ";":
      buf.extend(back_buf[:-1])
      buf.extend((EndOfStatement, "}"))
      del back_buf[1:] # now contains just 1 semi
      return semi

    else:
      buf.extend(back_buf)
      buf.extend((EndOfStatement, t))
      del back_buf[:]
      return dispatch

  back_buf = []
  buf = []
  state = dispatch
  for t in tokens:
    state = state()
    yield from buf
    del buf[:]

  yield from buf
  yield from back_buf




import sys
import os
from time import perf_counter

if __name__ == "__main__":
  filenames = sys.argv[1:]

  if len(filenames) == 0:
    if os.isatty(sys.stdin.fileno()):
      sys.exit("please provide a list of files")

    else:
      # input from pipe
      filenames = sys.stdin

  for filename in filenames:
    filename = filename.strip()
    if filename == "":
      continue

    try:
      with open(filename, 'r') as stream:
        stream = TextReader(stream)

        # t0 = perf_counter()
        tokens = tokenize(stream)
        tokens = join_tokens(tokens)
        tokens = no_preproc(tokens)

        # tokens = join_template_expressions(tokens)
        # tokens = statement_split(tokens)


        tokens = make_expressions(tokens)
        write_tokens(sys.stdout, tokens)
        # write_tokens(open('nul', 'w'), tokens)

        # functions = parse_functions(tokens, stream)
        # write_functions(sys.stdout, functions, filename, cpp2=True)

        # print(f"all took {perf_counter() - t0}")

    except (FileNotFoundError, OSError):
      sys.stderr.write(f"could not find {filename}\n")
    except UnicodeDecodeError:
      sys.stderr.write(f"decode error in {filename}\n")

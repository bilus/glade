import gleam/function
import gleam/int
import gleam/list
import gleam/order
import gleam/regexp
import gleam/result
import gleam/set
import gleam/string
import nibble/lexer

// type Abc(a, b) = Simple(a) | Complex(a, b)

pub type Token {
  TypeKeyword
  TypeName(String)
  GenericTypeName(String)
  Pipe
  Eq
  LParen
  RParen
  Comma
  Indent(Int)
  Unindent(Int)
}

type LexerMode {
  LexerMode(indent: Int)
}

pub opaque type Lexer {
  Lexer(impl: lexer.Lexer(Token, LexerMode))
}

pub type Error =
  lexer.Error

pub fn new() -> Lexer {
  lexer.advanced(fn(_) {
    [
      indentation(),
      lexer.keyword("type", "\\s+", TypeKeyword),
      lexer.identifier("[A-Za-z]", "[A-Za-z0-9_]", set.new(), TypeName),
      lexer.identifier("[a-z]", "[A-Za-z0-9_]", set.new(), GenericTypeName),
      lexer.token("|", Pipe),
      lexer.token("=", Eq),
      lexer.token("(", LParen),
      lexer.token(")", RParen),
      lexer.token(",", Comma),
      lexer.whitespace(Nil)
        |> lexer.ignore,
    ]
  })
  |> Lexer
}

fn indentation() -> lexer.Matcher(Token, LexerMode) {
  let assert Ok(is_indent) = regexp.from_string("^\\n[ \\t]*")
  use current_mode: LexerMode, lexeme, lookahead <- lexer.custom

  case regexp.check(is_indent, lexeme), lookahead {
    False, _ -> lexer.NoMatch
    True, " " | True, "\t" -> lexer.Skip
    True, "\n" -> lexer.Drop(current_mode)
    True, _ -> {
      let spaces = string.length(lexeme) - 1
      let mode = LexerMode(indent: spaces)

      case int.compare(spaces, current_mode.indent) {
        order.Lt -> lexer.Keep(Unindent(spaces), mode)
        order.Eq if spaces == 0 -> lexer.Drop(mode)
        order.Eq -> lexer.Keep(Indent(spaces), mode)
        order.Gt -> lexer.Keep(Indent(spaces), mode)
      }
    }
  }
}

pub fn run(lx: Lexer, source: String) -> Result(List(Token), Error) {
  source
  |> lexer.run_advanced(LexerMode(0), lx.impl)
  |> result.map(list.map(_, to_token))
}

fn to_token(lexer_token: lexer.Token(Token)) -> Token {
  let lexer.Token(_, _, value) = lexer_token
  value
}

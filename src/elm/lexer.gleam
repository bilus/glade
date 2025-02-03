import gleam/int
import gleam/order
import gleam/regexp
import gleam/set
import gleam/string
import nibble/lexer

pub type Token {
  TypeKeyword
  TypeName(String)
  GenericTypeName(String)
  VerticalBar
  Eq
  LParen
  RParen
  Comma
  Indent
  //(Int)
  Dedent
  //(Int)
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
      lexer.identifier("[A-Z]", "[A-Za-z0-9_]", set.new(), TypeName),
      lexer.identifier("[a-z]", "[A-Za-z0-9_]", set.new(), GenericTypeName),
      lexer.token("|", VerticalBar),
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
  // TODO: Generate pairs of Indent - Dedent
  // properly, matching the indentation levels.
  // Write tests

  // TODO: Add Dedent token for EOF, if it isn't already there
  // (no newline at the end of the file)
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
        order.Lt -> lexer.Keep(Dedent, mode)
        order.Eq if spaces == 0 -> lexer.Drop(mode)
        order.Eq -> lexer.Drop(current_mode)
        order.Gt -> lexer.Keep(Indent, mode)
      }
    }
  }
}

pub fn run(lx: Lexer, source: String) -> Result(List(lexer.Token(Token)), Error) {
  source
  |> lexer.run_advanced(LexerMode(0), lx.impl)
}

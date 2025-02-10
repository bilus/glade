import gleam/list
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import nibble/lexer

pub type Token {
  // TODO: Reorg
  //-----------------------------------
  // Existing tokens
  //-----------------------------------
  TypeKeyword
  UnitKeyword
  UpcaseIdentifier(String)
  VerticalBar
  Eq
  LParen
  RParen
  Comma
  Comment(String)

  //-----------------------------------
  // Layout keywords
  //-----------------------------------
  LetKeyword
  InKeyword
  CaseKeyword
  OfKeyword
  WhereKeyword
  DoKeyword

  //-----------------------------------
  // Composite keywords
  //-----------------------------------
  TypeAliasKeyword
  PortModuleKeyword
  ModuleKeyword

  //-----------------------------------
  // Basic keywords
  //-----------------------------------
  IfKeyword
  ThenKeyword
  ElseKeyword
  ImportKeyword
  ExposingKeyword
  AsKeyword

  //-----------------------------------
  // Values
  //-----------------------------------
  BoolLiteral(Bool)
  FloatLiteral(Float)
  IntLiteral(Int)
  StringLiteral(String)
  Identifier(String)

  //-----------------------------------
  // Operators
  //-----------------------------------
  /// ->
  Arrow
  /// |>
  Pipeline
  /// <|
  ReverseApply
  /// ::
  Cons
  /// ++
  Concat

  //-----------------------------------
  // Mathematical/Comparison operators
  //-----------------------------------
  Plus
  Minus
  Multiply
  Divide
  /// //
  IntDivide
  /// ^
  Power
  Modulo
  /// ==
  Equal
  /// /=
  NotEqual
  LessThan
  GreaterThan
  LessEqual
  GreaterEqual

  //-----------------------------------
  // Additional structural tokens
  //-----------------------------------
  LBracket
  RBracket
  LBrace
  RBrace
  Dot
  /// ..
  DoubleDot
  Colon

  //-----------------------------------
  // Layout tokens (for our processor)
  //-----------------------------------
  /// Generated { for layout
  LayoutStart
  /// Generated } for layout
  LayoutEnd
  /// Generated ; for layout
  LayoutSemicolon
}

type LexerState {
  LexerState
}

pub opaque type Lexer {
  Lexer(impl: lexer.Lexer(Token, LexerState))
}

pub type Error =
  lexer.Error

pub fn new() -> Lexer {
  lexer.advanced(fn(_) {
    [
      // Comment handling
      multi_line_comment() |> lexer.ignore,
      single_line_comment() |> lexer.ignore,
      // Layout keywords
      lexer.keyword("let", "\\s+", LetKeyword),
      lexer.keyword("in", "\\s+", InKeyword),
      lexer.keyword("case", "\\s+", CaseKeyword),
      lexer.keyword("of", "\\s+", OfKeyword),
      lexer.keyword("where", "\\s+", WhereKeyword),
      lexer.keyword("do", "\\s+", DoKeyword),
      // Composite keywords
      lexer.keyword("type alias", "\\s+", TypeAliasKeyword),
      lexer.keyword("port module", "\\s+", PortModuleKeyword),
      lexer.keyword("module", "\\s+", ModuleKeyword),
      // Basic keywords
      lexer.keyword("type", "\\s+", TypeKeyword),
      lexer.keyword("if", "\\s+", IfKeyword),
      lexer.keyword("then", "\\s+", ThenKeyword),
      lexer.keyword("else", "\\s+", ElseKeyword),
      lexer.keyword("import", "\\s+", ImportKeyword),
      lexer.keyword("exposing", "\\s+", ExposingKeyword),
      lexer.keyword("as", "\\s+", AsKeyword),
      // Boolean literals
      lexer.keyword("True", "\\b", BoolLiteral(True)),
      lexer.keyword("False", "\\b", BoolLiteral(False)),
      // Operators (multi-character)
      lexer.token("->", Arrow),
      lexer.token("|>", Pipeline),
      lexer.token("<|", ReverseApply),
      lexer.token("::", Cons),
      lexer.token("++", Concat),
      lexer.token("//", IntDivide),
      lexer.token("==", Equal),
      lexer.token("/=", NotEqual),
      lexer.token("<=", LessEqual),
      lexer.token(">=", GreaterEqual),
      lexer.token("..", DoubleDot),
      // Operators (single-character)
      lexer.token("+", Plus),
      lexer.symbol("-", "\\s", Minus),
      lexer.token("*", Multiply),
      lexer.token("/", Divide),
      lexer.token("^", Power),
      lexer.token("%", Modulo),
      lexer.token("<", LessThan),
      lexer.token(">", GreaterThan),
      lexer.token("=", Eq),
      lexer.symbol(".", "\\s", Dot),
      lexer.token("|", VerticalBar),
      // Structural tokens
      lexer.token("(", LParen),
      lexer.token(")", RParen),
      lexer.token("[", LBracket),
      lexer.token("]", RBracket),
      lexer.token("{", LBrace),
      lexer.token("}", RBrace),
      lexer.token(":", Colon),
      lexer.token(",", Comma),
      // Identifiers and literals
      lexer.identifier("[A-Z]", "[A-Za-z0-9_]", set.new(), UpcaseIdentifier),
      lexer.identifier("[a-z]", "[A-Za-z0-9_]", set.new(), Identifier),
      // Number literals (both integer and float)
      lexer.number(IntLiteral, FloatLiteral),
      // TODO: Properly handle string, esp. multi-line strings.
      // String literals
      lexer.string("\"", StringLiteral),
      // Multiline strings (triple quoted)
      lexer.string("\"\"\"", StringLiteral),
      // Whitespace handling
      lexer.whitespace(Nil)
        |> lexer.ignore,
    ]
  })
  |> Lexer
}

pub fn run(lx: Lexer, source: String) -> Result(List(lexer.Token(Token)), Error) {
  source
  |> lexer.run_advanced(LexerState, lx.impl)
  |> result.map(indentation_to_layout)
}

pub fn single_line_comment() -> lexer.Matcher(Token, mode) {
  let start = "--"
  use mode, lexeme, lookahead <- lexer.custom()

  case string.starts_with(lexeme, start), lookahead {
    True, "\n" ->
      lexeme
      |> string.drop_start(string.length(start))
      |> Comment
      |> lexer.Keep(mode)
    True, _ -> lexer.Skip
    False, _ -> lexer.NoMatch
  }
}

pub fn multi_line_comment() -> lexer.Matcher(Token, mode) {
  let start = "{-"
  let end = "-}"
  use mode, lexeme, lookahead <- lexer.custom()

  case
    string.starts_with(lexeme, start),
    string.ends_with(lexeme, end),
    lexeme <> lookahead
  {
    True, True, _ ->
      lexeme
      |> string.drop_start(string.length(start))
      |> string.drop_end(string.length(end))
      |> Comment
      |> lexer.Keep(mode)
    True, _, _ -> lexer.Skip
    _, _, "{-" -> lexer.Skip
    False, _, _ -> lexer.NoMatch
  }
}

type LayoutBlock {
  LayoutBlock(col: Int, row: Int)
}

fn flat_map_fold(
  over list: List(a),
  from initial: b,
  with fun: fn(b, a) -> #(b, List(c)),
) -> List(c) {
  list
  |> list.map_fold(initial, fun)
  |> pair.second
  |> list.flatten
}

fn indentation_to_layout(
  tokens: List(lexer.Token(Token)),
) -> List(lexer.Token(Token)) {
  let updated_tokens =
    tokens
    |> flat_map_fold([], fn(layouts, token) {
      case is_layout_end(layouts, token), is_layout_start(token) {
        True, True -> #(layouts |> pop_layout |> push_layout(token), [
          layout_token(LayoutEnd, after: token),
          token,
          layout_token(LayoutStart, after: token),
        ])
        False, True -> #(layouts |> push_layout(token), [
          token,
          layout_token(LayoutStart, after: token),
        ])
        True, False -> #(layouts |> pop_layout, [
          token,
          layout_token(LayoutEnd, after: token),
        ])
        False, False -> #(layouts, [token])
      }
    })
  case list.last(updated_tokens) {
    Error(Nil) -> []
    Ok(lexer.Token(_, _, LayoutEnd)) -> updated_tokens
    Ok(last) ->
      updated_tokens |> list.append([layout_token(LayoutEnd, after: last)])
  }
}

fn is_layout_start(token: lexer.Token(Token)) -> Bool {
  let lexer.Token(span: _, lexeme: _, value: value) = token
  case value {
    TypeKeyword | ModuleKeyword -> True
    _ -> False
  }
}

fn is_layout_end(layouts: List(LayoutBlock), token: lexer.Token(Token)) -> Bool {
  case layouts {
    [] -> False
    [recent, ..] -> {
      let lexer.Token(span: span, lexeme: _, value: _) = token
      span.row_start > recent.row && span.col_start == 1
    }
  }
}

fn pop_layout(layouts: List(LayoutBlock)) -> List(LayoutBlock) {
  layouts |> list.drop(1)
}

fn push_layout(
  layouts: List(LayoutBlock),
  token: lexer.Token(Token),
) -> List(LayoutBlock) {
  let lexer.Token(span: span, lexeme: _, value: _) = token
  let layout = LayoutBlock(col: span.col_start, row: span.row_start)
  layouts |> list.prepend(layout)
}

/// Construct a layout token after the given token. For example to mark the
/// start of a layout block, e.g. after the type keyword.
fn layout_token(
  token: tok,
  after previous_token: lexer.Token(tok),
) -> lexer.Token(tok) {
  let row = previous_token.span.row_end
  let col = previous_token.span.col_end
  lexer.Token(
    span: lexer.Span(row_start: row, row_end: row, col_start: col, col_end: col),
    lexeme: "",
    value: token,
  )
}

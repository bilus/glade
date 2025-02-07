import debug
import gleam/list
import gleam/option.{type Option, None, Some}
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
  TypeName(String)
  VerticalBar
  Eq
  LParen
  RParen
  Comma

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
      // // Indentation handling
      // indentation(),
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
      lexer.token("-", Minus),
      lexer.token("*", Multiply),
      lexer.token("/", Divide),
      lexer.token("^", Power),
      lexer.token("%", Modulo),
      lexer.token("<", LessThan),
      lexer.token(">", GreaterThan),
      lexer.token("=", Eq),
      lexer.token(".", Dot),
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
      lexer.identifier("[A-Z]", "[A-Za-z0-9_]", set.new(), TypeName),
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
  |> lexer.run_advanced(LexerMode(0), lx.impl)
  |> result.map(indentation_to_layout)
}

type LayoutContext {
  LayoutContext(col: Int, row: Int)
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
    |> flat_map_fold(None, fn(ctx, token) {
      let lexer.Token(_, _, value) = token
      debug.log("TOKEN: " <> string.inspect(token))
      let updated_context = update_layout_context(ctx, token)
      case value, is_layout_end(updated_context, ctx) {
        TypeKeyword, True -> #(Some(updated_context), [
          layout_token(LayoutEnd, after: token),
          token,
          layout_token(LayoutStart, after: token),
        ])
        TypeKeyword, False -> #(Some(updated_context), [
          token,
          layout_token(LayoutStart, after: token),
        ])
        _, True -> #(Some(updated_context), [
          token,
          layout_token(LayoutEnd, after: token),
        ])
        _, False -> #(Some(updated_context), [token])
      }
    })
  case list.last(updated_tokens) {
    Error(Nil) -> []
    Ok(lexer.Token(_, _, LayoutEnd)) -> updated_tokens
    Ok(last) ->
      updated_tokens |> list.append([layout_token(LayoutEnd, after: last)])
  }
}

fn update_layout_context(
  _: Option(LayoutContext),
  token: lexer.Token(a),
) -> LayoutContext {
  let lexer.Token(span, _, _) = token
  LayoutContext(col: span.col_start, row: span.row_start)
}

fn is_layout_end(ctx: LayoutContext, prev_ctx: Option(LayoutContext)) -> Bool {
  debug.log(string.inspect(ctx) <> " <---- " <> string.inspect(prev_ctx))
  // TODO: This is a hack. What we should do, long term is keep a stack
  // of layout context for every layout keyword we encounter
  // and track indentation relative to the column of the layout keyword.
  case prev_ctx {
    Some(prev_ctx) -> ctx.row > prev_ctx.row && ctx.col == 1
    None -> False
  }
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

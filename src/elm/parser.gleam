import debug
import elm/ast
import elm/lexer
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import nibble.{
  backtrackable, do, eof, fail, lazy, many, one_of, optional, replace, sequence,
  succeed, take_map, token,
}

pub type Parser(a, ctx) =
  nibble.Parser(a, lexer.Token, ctx)

pub type Error(ctx) {
  NoMatchingTokenError(row: Int, col: Int, lexeme: String)
  ParserError(List(nibble.DeadEnd(lexer.Token, ctx)))
}

fn type_name() -> Parser(ast.TypeName, ctx) {
  {
    use name <- take_map("Expected type name")
    case name {
      lexer.UpcaseIdentifier(name) -> Some(ast.TypeName(name))
      _ -> None
    }
  }
  |> inspect("DONE: type_name")
}

fn generic_type_annotation() -> Parser(ast.GenericName, ctx) {
  {
    use name <- take_map("Expected a generic type")
    case name {
      lexer.Identifier(name) -> Some(name)
      _ -> None
    }
  }
  |> inspect("DONE: generic_type_annotation")
}

fn value_constructor() -> Parser(ast.ValueConstructor, ctx) {
  use name <- do(type_name())
  use arguments <- do(many(
    lazy(type_annotation_term) |> inspect("type_annotation_term"),
  ))
  succeed(ast.ValueConstructor(name, arguments))
  |> inspect("DONE: value_constructor")
}

fn type_annotation() -> Parser(ast.TypeAnnotation, ctx) {
  one_of([
    function_type_annotation()
      |> backtrackable()
      |> inspect("function_type_annotation"),
    typed_annotation() |> backtrackable() |> inspect("typed_annotation"),
    type_annotation_term()
      |> backtrackable()
      |> inspect("type_annotation_term"),
  ])
}

fn function_type_annotation() -> nibble.Parser(
  ast.TypeAnnotation,
  lexer.Token,
  ctx,
) {
  use arg <- do(type_annotation_term())
  use _ <- do(token(lexer.Arrow))
  use ret <- do(type_annotation())
  succeed(ast.FunctionType(arg, ret))
  |> inspect("DONE: function_type_annotation")
}

fn parens_type_annotation() -> nibble.Parser(
  ast.TypeAnnotation,
  lexer.Token,
  ctx,
) {
  use _ <- do(token(lexer.LParen))
  use type_annotation <- do(type_annotation() |> inspect("type_annotation"))
  use _ <- do(token(lexer.RParen))
  succeed(type_annotation) |> inspect("DONE: parens_type_annotation")
}

fn type_annotation_term() -> Parser(ast.TypeAnnotation, ctx) {
  one_of([
    generic_type_annotation()
      |> nibble.map(ast.GenericType)
      |> inspect("generic_name"),
    typed_annotation_no_arguments()
      |> inspect("typed_annotation_no_arguments"),
    // TODO: Optimize performance by consuming ( and then
    // handling either tuple or unit.
    tupled_type_annotation()
      |> backtrackable()
      |> inspect("tupled_type_annotation"),
    unit() |> replace(ast.Unit) |> backtrackable() |> inspect("unit"),
    parens_type_annotation()
      |> backtrackable()
      |> inspect("parens_type_annotation"),
    record_type_annotation()
      |> backtrackable()
      |> inspect("record_type_annotation"),
  ])
}

fn typed_annotation_no_arguments() -> nibble.Parser(
  ast.TypeAnnotation,
  lexer.Token,
  ctx,
) {
  use type_name <- do(type_name())
  succeed(ast.Typed(type_name, []))
  |> inspect("DONE: typed_annotation_no_arguments")
}

pub fn record_type_annotation() -> nibble.Parser(
  ast.TypeAnnotation,
  lexer.Token,
  ctx,
) {
  use _ <- do(token(lexer.LBrace))

  use generic_name <- do(optional(generic_record_type()) |> backtrackable())

  use fields <- do(sequence(record_field(), token(lexer.Comma)))

  use _ <- do(token(lexer.RBrace))

  succeed(case generic_name {
    Some(name) -> ast.GenericRecord(name, ast.RecordDefinition(fields))
    None -> ast.Record(ast.RecordDefinition(fields))
  })
  |> inspect("DONE: record_type_annotation")
}

fn record_field() -> nibble.Parser(ast.RecordField, lexer.Token, ctx) {
  use name <- do(record_field_name())
  use _ <- do(token(lexer.Colon))
  use type_annotation <- do(inspect(type_annotation(), "type_annotation"))
  debug.log(type_annotation)
  succeed(ast.RecordField(name, type_annotation))
  |> inspect("DONE: record_field")
}

fn generic_record_type() {
  use generic_name <- do(generic_type_annotation())
  use _ <- do(token(lexer.VerticalBar))
  succeed(generic_name)
}

fn record_field_name() -> nibble.Parser(ast.RecordFieldName, lexer.Token, ctx) {
  use name <- take_map("Expected record field name")
  case name {
    lexer.Identifier(name) -> Some(name)
    _ -> None
  }
}

fn unit() -> nibble.Parser(Nil, lexer.Token, b) {
  use _ <- do(token(lexer.LParen))
  use _ <- do(token(lexer.RParen))
  succeed(Nil)
}

fn tupled_type_annotation() -> nibble.Parser(
  ast.TypeAnnotation,
  lexer.Token,
  ctx,
) {
  use _ <- do(token(lexer.LParen))
  use type_annotations <- do(sequence(type_annotation(), token(lexer.Comma)))
  use _ <- do(token(lexer.RParen))
  case type_annotations {
    [] -> fail("Not a tuple")
    [_] -> fail("Not a tuple")
    _ ->
      succeed(ast.Tupled(type_annotations))
      |> inspect("DONE: tupled_type_annotation")
  }
}

fn typed_annotation() -> nibble.Parser(ast.TypeAnnotation, lexer.Token, ctx) {
  use type_name <- do(type_name())
  use type_annotations <- do(many(
    type_annotation_term() |> inspect("type_annotation_term"),
  ))
  succeed(ast.Typed(type_name, type_annotations))
  |> inspect("DONE: typed_annotation")
}

fn custom_type() -> Parser(ast.Type, ctx) {
  use _ <- do(token(lexer.TypeKeyword))
  use _ <- do(layout_start())
  use type_name <- do(type_name())
  use generics <- do(many(generic_type_annotation()))
  use _ <- do(token(lexer.Eq))
  use constructors <- do(sequence(
    lazy(value_constructor),
    token(lexer.VerticalBar),
  ))
  use _ <- do(layout_end())
  succeed(ast.Type(type_name, generics, constructors))
  |> inspect("DONE: custom_type")
}

fn layout_start() {
  token(lexer.LayoutStart)
}

fn layout_end() {
  token(lexer.LayoutEnd)
}

pub fn module() -> Parser(ast.Module, ctx) {
  use _ <- do(token(lexer.ModuleKeyword) |> inspect("module_keyword"))
  use _ <- do(layout_start())
  use name <- do(module_name() |> inspect("module_name"))
  use exposing <- do(nibble.or(
    exposing() |> inspect("exposing"),
    ast.ExposingNothing,
  ))
  use _ <- do(layout_end())
  use declarations <- do(many(
    custom_type_declaration() |> inspect("custom_type_declaration"),
  ))
  use _ <- do(eof())
  succeed(ast.Module(name, declarations:, exposing:))
}

fn exposing() -> Parser(ast.Exposing, ctx) {
  use _ <- do(token(lexer.ExposingKeyword))
  use _ <- do(token(lexer.LParen))
  use exposed <- do(
    one_of([
      expose_all(),
      sequence(
        top_level_expose() |> inspect("exposed_value"),
        token(lexer.Comma),
      )
        |> nibble.map(ast.Explicit),
    ]),
  )
  use _ <- do(token(lexer.RParen))
  succeed(exposed)
  |> inspect("DONE: exposing")
}

fn top_level_expose() -> Parser(ast.TopLevelExpose, ctx) {
  one_of([
    type_expose() |> inspect("type_expose"),
    function_expose() |> inspect("function_expose"),
  ])
}

fn type_expose() -> nibble.Parser(ast.TopLevelExpose, lexer.Token, ctx) {
  use name <- do(type_name())
  use is_opaque <- do(
    optional({
      use _ <- do(token(lexer.LParen))
      use _ <- do(token(lexer.DoubleDot))
      use _ <- do(token(lexer.RParen))
      succeed(False)
    })
    |> nibble.map(option.unwrap(_, True)),
  )
  succeed(ast.TypeOrAliasExpose(name, is_opaque))
}

fn function_expose() -> Parser(ast.TopLevelExpose, ctx) {
  {
    use tok <- take_map("Expected function or type name")
    case tok {
      lexer.Identifier(name) -> Some(ast.FunctionExpose(ast.FunctionName(name)))
      _ -> None
    }
  }
  |> inspect("DONE: top_level_expose")
}

fn expose_all() -> Parser(ast.Exposing, ctx) {
  use _ <- do(token(lexer.DoubleDot))
  succeed(ast.ExposingAll)
  |> inspect("DONE: expose_all")
}

fn module_name() -> nibble.Parser(String, lexer.Token, ctx) {
  use tok <- take_map("Expected module name")
  case tok {
    lexer.UpcaseIdentifier(name) -> Some(name)
    _ -> None
  }
}

fn custom_type_declaration() -> nibble.Parser(ast.Declaration, lexer.Token, ctx) {
  custom_type()
  |> nibble.map(ast.CustomTypeDeclaration)
}

pub fn parse(
  elm_source: String,
  parser: Parser(a, ctx),
) -> Result(a, Error(ctx)) {
  use tokens <- result.try(
    lexer.new()
    |> lexer.run(elm_source)
    |> result.map_error(fn(e) {
      NoMatchingTokenError(row: e.row, col: e.col, lexeme: e.lexeme)
    }),
  )
  debug.log("*** TOKENS ***")
  tokens
  |> list.each(debug.log)
  tokens
  |> nibble.run(parser)
  |> result.map_error(ParserError)
}

pub fn inspect(parser: Parser(a, ctx), msg: String) -> Parser(a, ctx) {
  case debug.enabled() {
    True -> {
      // debug.log("* Inspecting Parser")
      let parser = nibble.inspect(parser, msg)
      parser
    }
    False -> parser
  }
}

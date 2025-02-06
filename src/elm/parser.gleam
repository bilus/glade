import elm/ast
import elm/lexer
import gleam/option.{None, Some}
import gleam/result
import nibble.{
  backtrackable, do, fail, lazy, many, many1, one_of, replace, sequence, succeed,
  take_map, token,
}

pub type Parser(a, ctx) =
  nibble.Parser(a, lexer.Token, ctx)

pub type Error(ctx) {
  LexerError(lexer.Error)
  ParserError(List(nibble.DeadEnd(lexer.Token, ctx)))
}

fn type_name() -> Parser(ast.TypeName, ctx) {
  use name <- take_map("Expected type name")
  case name {
    lexer.TypeName(name) -> Some(ast.TypeName(name))
    _ -> None
  }
}

fn generic_name() -> Parser(ast.GenericName, ctx) {
  use name <- take_map("Expected a generic type")
  case name {
    lexer.GenericTypeName(name) -> Some(name)
    _ -> None
  }
}

fn value_constructor() -> Parser(ast.ValueConstructor, ctx) {
  use name <- do(type_name())
  use arguments <- do(many(lazy(type_annotation)))
  succeed(ast.ValueConstructor(name, arguments))
}

fn type_annotation() -> Parser(ast.TypeAnnotation, ctx) {
  one_of([
    generic_name()
      |> nibble.map(ast.GenericType),
    // TODO: Optimize performance by consuming ( and then
    // handling either tuple or unit.
    tupled_type_annotation() |> backtrackable(),
    unit() |> replace(ast.Unit) |> backtrackable(),
    typed_annotation_with_arguments(),
    typed_annotation_no_arguments(),
  ])
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
    _ -> succeed(ast.Tupled(type_annotations))
  }
}

fn typed_annotation_with_arguments() -> nibble.Parser(
  ast.TypeAnnotation,
  lexer.Token,
  ctx,
) {
  use _ <- do(token(lexer.LParen))
  use type_name <- do(type_name())
  use type_annotations <- do(many1(type_annotation()))
  use _ <- do(token(lexer.RParen))
  succeed(ast.Typed(type_name, type_annotations))
}

fn typed_annotation_no_arguments() -> nibble.Parser(
  ast.TypeAnnotation,
  lexer.Token,
  ctx,
) {
  use type_name <- do(type_name())
  succeed(ast.Typed(type_name, []))
}

fn custom_type() -> Parser(ast.Type, ctx) {
  use _ <- do(token(lexer.TypeKeyword))
  use _ <- do(layout_start())
  use type_name <- do(type_name())
  use generics <- do(many(generic_name()))
  use _ <- do(token(lexer.Eq))
  use constructors <- do(sequence(
    lazy(value_constructor),
    token(lexer.VerticalBar),
  ))
  use _ <- do(layout_end())
  succeed(ast.Type(type_name, generics, constructors))
}

fn layout_start() {
  token(lexer.LayoutStart)
}

fn layout_end() {
  token(lexer.LayoutEnd)
}

pub fn module() -> Parser(ast.Module, ctx) {
  use custom_types <- do(many(custom_type_declaration()))
  succeed(ast.Module(declarations: custom_types))
}

fn custom_type_declaration() -> nibble.Parser(ast.Declaration, lexer.Token, ctx) {
  custom_type()
  |> nibble.map(ast.CustomTypeDeclaration)
}

pub fn run(elm_source: String, parser: Parser(a, ctx)) -> Result(a, Error(ctx)) {
  use tokens <- result.try(
    lexer.new()
    |> lexer.run(elm_source)
    |> result.map_error(LexerError),
  )
  tokens
  |> nibble.run(parser)
  |> result.map_error(ParserError)
}

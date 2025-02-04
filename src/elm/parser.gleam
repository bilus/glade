import elm/ast
import elm/lexer
import gleam/option.{None, Some, is_some}
import gleam/result
import nibble.{
  do, guard, lazy, many, optional, return, sequence, take_map, token,
}

pub type Parser(a, ctx) =
  nibble.Parser(a, lexer.Token, ctx)

pub type Error(ctx) {
  LexerError(lexer.Error)
  ParserError(List(nibble.DeadEnd(lexer.Token, ctx)))
}

fn type_name_parser() -> Parser(ast.TypeName, ctx) {
  use name <- take_map("Expected type name")
  case name {
    lexer.TypeName(name) -> Some(ast.TypeName(name))
    _ -> None
  }
}

fn generic_name_parser() -> Parser(ast.GenericName, ctx) {
  use name <- take_map("Expected a generic type")
  case name {
    lexer.GenericTypeName(name) -> Some(name)
    _ -> None
  }
}

fn value_constructor_parser() -> Parser(ast.ValueConstructor, ctx) {
  use name <- do(type_name_parser())
  use arguments <- do(many(lazy(type_annotation_parser)))
  return(ast.ValueConstructor(name, arguments))
}

fn type_annotation_parser() -> Parser(ast.TypeAnnotation, ctx) {
  generic_name_parser()
  |> nibble.map(ast.GenericType)
}

pub fn custom_type_parser() -> Parser(ast.Type, ctx) {
  use _ <- do(token(lexer.TypeKeyword))
  use type_name <- do(type_name_parser())
  use generics <- do(many(generic_name_parser()))
  use indent <- do(optional(token(lexer.Indent)))
  use _ <- do(token(lexer.Eq))
  use constructors <- do(sequence(
    lazy(value_constructor_parser),
    token(lexer.VerticalBar),
  ))
  use dedent <- do(optional(token(lexer.Dedent)))
  use _ <- do(guard(is_some(indent) == is_some(dedent), "Incorrect indentation"))
  return(ast.Type(type_name, generics, constructors))
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

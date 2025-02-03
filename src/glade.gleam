import decepticon/stateful as state
import elm/lexer
import glance
import glance_printer as glpr
import gleam/io
import gleam/list
import gleam/option.{None, Some, is_some}
import gleam/result

// import gleamgen/expression/constructor
// import gleamgen/module
// import gleamgen/render
// import gleamgen/types
// import gleamgen/types/custom
// import gleamgen/types/variant
import gleam/string_tree as st
import nibble.{
  do, guard, lazy, many, many1, optional, return, sequence, take_map, token,
}

pub type TypeName {
  TypeName(name: String)
}

pub type Generic =
  String

pub type Type {
  Type(
    name: TypeName,
    generics: List(Generic),
    constructor: List(ValueConstructor),
  )
}

pub type ValueConstructor {
  ValueConstructor(name: TypeName, arguments: List(TypeAnnotation))
}

// type TypeAnnotation
//     = GenericType String
//     | Typed
//           (Node ( ModuleName, String ))
//           (List (Node TypeAnnotation))
//     | Unit
//     | Tupled (List (Node TypeAnnotation))
//     | Record RecordDefinition
//     | GenericRecord (Node String) (Node RecordDefinition)
//     | FunctionTypeAnnotation
//           (Node TypeAnnotation)
//           (Node TypeAnnotation)
// Custom type for different type annotations. For example:

// GenericType: a
// Typed: Maybe (Int -> String)
// Unit: ()
// Tuples: (a, b, c)
// Record: { name : String}
// GenericRecord: { a | name : String}
// FunctionTypeAnnotation: Int -> String

pub type TypeAnnotation {
  GenericType(Generic)
}

type Context =
  Nil

pub type Error(e) {
  LexerError(lexer.Error)
  ParserError(e)
}

fn parse_type_name() {
  use name <- take_map("Expected type name")
  case name {
    lexer.TypeName(name) -> Some(TypeName(name))
    _ -> None
  }
}

fn parse_generic() {
  use name <- take_map("Expected a generic type")
  case name {
    lexer.GenericTypeName(name) -> Some(name)
    _ -> None
  }
}

fn parse_value_constructor() {
  use name <- do(parse_type_name())
  use arguments <- do(many(lazy(parse_type_annotation)))
  return(ValueConstructor(name, arguments))
}

fn parse_type_annotation() {
  parse_generic()
  |> nibble.map(GenericType)
}

fn parse_type() -> nibble.Parser(Type, lexer.Token, Context) {
  use _ <- do(token(lexer.TypeKeyword))
  use type_name <- do(parse_type_name())
  use generics <- do(many(parse_generic()))
  use indent <- do(optional(token(lexer.Indent)))
  use _ <- do(token(lexer.Eq))
  use constructors <- do(sequence(
    lazy(parse_value_constructor),
    token(lexer.VerticalBar),
  ))
  use dedent <- do(optional(token(lexer.Dedent)))
  use _ <- do(guard(is_some(indent) == is_some(dedent), "Incorrect indentation"))
  return(Type(type_name, generics, constructors))
}

pub fn main2() {
  let src =
    "
type Maybe a
  = Just a
  | Nothing
"
  io.debug("PARSING")
  io.debug(src)
  io.debug("-----")
  let parser = parse_type()

  use tokens <- result.try(
    lexer.new()
    |> lexer.run(src)
    |> result.map_error(LexerError),
  )
  io.println("TOKENS")
  io.debug(tokens)
  io.println("-----")
  let result =
    tokens
    |> nibble.run(parser)
    |> result.map_error(ParserError)
  io.println("RESULT")
  io.debug(result)
  use ast <- result.try(result)
  use gleam_ast <- result.try(gleam_custom_type(ast, True))
  io.debug(gleam_ast)
  let gleam_module = glance.Module([], [gleam_ast], [], [], [])
  io.debug(glpr.print(gleam_module))
  Ok("")
}

pub fn main() {
  // let src = "type Maybe a = Just a | Nothing"

  // use ast <- result.try(result)
  // let string_tree = state.exec(gleam_type(ast, True), st.new())
  // io.debug(st.to_string(string_tree))

  use ast <- result.try(glance.module(
    "pub type Maybe(a) {\n  Nothing\n   Just(a)\n}",
  ))
  io.debug(ast)

  // let custom_type =
  //   custom.new(MyCustomTypeReference)
  //   |> custom.with_generic("a")
  //   |> custom.with_unchecked_variants(fn(generics) {
  //     let #(#(), a) = generics
  //     [
  //       variant.new("Nothing") |> variant.to_unchecked(),
  //       variant.new("Just")
  //         |> variant.with_argument(None, a)
  //         |> variant.to_unchecked(),
  //     ]
  //   })

  // let mod = {
  //   use custom_type_type, custom_constructors <- module.with_custom_type_unchecked(
  //     module.DefinitionDetails(name: "Maybe", is_public: True, attributes: []),
  //     custom_type,
  //   )
  //   module.eof()
  // }
  // mod
  // |> module.render(render.default_context())
  // |> render.to_string()
  // |> io.debug
  Ok(GenericType("a"))
}

// fn generate_type(
//   type_: Type,
//   handler: fn(
//     custom.CustomType(a, b),
//     List(constructor.Constructor(a, types.Unchecked, b)),
//   ) ->
//     module.Module,
// ) -> module.Module {
//   let custom_type =
//     custom.new(type_)
//     // |> list.fold(type_.generics, _, fn(custom_type, generic) {
//     //   custom_type |> custom.with_generic("a")
//     // })
//     |> custom.with_generic("a")
//     |> custom.with_unchecked_variants(fn(generics) {
//       let #(#(), a) = generics
//       [
//         variant.new("Nothing") |> variant.to_unchecked(),
//         variant.new("Just")
//           |> variant.with_argument(None, a)
//           |> variant.to_unchecked(),
//       ]
//     })

//   use custom_type_type, custom_constructors <- module.with_custom_type_unchecked(
//     module.DefinitionDetails(name: "Maybe", is_public: True, attributes: []),
//     custom_type,
//   )
//   handler(custom_type_type, custom_constructors)
// }

// type StringBuilder(a) =
//   state.State(a, st.StringTree)

// fn modify(f: fn(s) -> s) -> state.State(s, s) {
//   state.State(run: fn(s) {
//     let s = f(s)
//     #(s, s)
//   })
// }

// fn emit(s: String) {
//   modify(st.append(_, s))
// }

// fn emit_newline() {
//   emit("\n")
// }

// fn emit_indent() {
//   emit("  ")
// }

// fn emit_keyword(s: String) {
//   use _ <- state.do(emit(s))
//   emit(" ")
// }

// fn mzero() {
//   emit("")
// }

// fn gleam_type(type_: Type, public: Bool) -> StringBuilder(_) {
//   let Type(name, generics, constructors) = type_
//   case public {
//     True -> emit_keyword("pub")
//     False -> mzero()
//   }
//   emit_type(name)
// }

fn gleam_custom_type(
  type_: Type,
  public: Bool,
) -> Result(glance.Definition(glance.CustomType), Error(e)) {
  let Type(TypeName(name), generics, constructors) = type_

  Ok(glance.Definition(
    [],
    glance.CustomType(
      name,
      case public {
        True -> glance.Public
        False -> glance.Private
      },
      False,
      generics,
      constructors
        |> list.map(gleam_variant),
    ),
  ))
}

fn gleam_variant(constructor: ValueConstructor) -> glance.Variant {
  let ValueConstructor(TypeName(name), arguments) = constructor
  glance.Variant(
    name,
    arguments
      |> list.map(fn(ann: TypeAnnotation) {
        case ann {
          GenericType(name) ->
            glance.UnlabelledVariantField(glance.VariableType(name))
        }
      }),
  )
}

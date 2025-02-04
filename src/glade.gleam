import elm/ast as elm
import elm/parser
import glance
import glance_printer as glpr
import gleam/io
import gleam/list
import gleam/result

pub type Error(ctx) {
  ElmParserError(parser.Error(ctx))
  GleamGeneratorError(String)
  GlanceError(glance.Error)
}

pub fn main() {
  let elm_src =
    "
type Maybe a
  = Just a
  | Nothing
"
  use elm_ast <- result.try(
    parser.run(elm_src, parser.custom_type_parser())
    |> result.map_error(ElmParserError),
  )
  use gleam_ast <- result.try(gleam_custom_type(elm_ast, True))
  let gleam_module = glance.Module([], [gleam_ast], [], [], [])
  let gleam_src = glpr.print(gleam_module)
  use gleam_ast2 <- result.try(
    glance.module(gleam_src) |> result.map_error(GlanceError),
  )
  io.debug(gleam_ast)
  io.debug(gleam_ast2)
  Ok("")
}

fn gleam_custom_type(
  type_: elm.Type,
  public: Bool,
) -> Result(glance.Definition(glance.CustomType), Error(ctx)) {
  let elm.Type(elm.TypeName(name), generics, constructors) = type_

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

fn gleam_variant(constructor: elm.ValueConstructor) -> glance.Variant {
  let elm.ValueConstructor(elm.TypeName(name), arguments) = constructor
  glance.Variant(
    name,
    arguments
      |> list.map(fn(ann: elm.TypeAnnotation) {
        case ann {
          elm.GenericType(name) ->
            glance.UnlabelledVariantField(glance.VariableType(name))
        }
      }),
  )
}

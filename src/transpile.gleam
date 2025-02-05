import elm/ast as elm
import glance
import glance_printer
import gleam/list
import gleam/option.{None}
import gleam/result

pub type Error {
  Error(glance.Error)
}

pub fn module(elm_ast: elm.Module) -> Result(glance.Module, Error) {
  use custom_types <- result.try(
    elm_ast.declarations
    |> list.map(fn(decl) {
      case decl {
        elm.CustomTypeDeclaration(custom_type) -> Ok(custom_type)
      }
    })
    |> result.values()
    |> list.map(fn(type_) { custom_type(type_, True) })
    |> result.all(),
  )
  let gleam_module = glance.Module([], custom_types, [], [], [])
  Ok(gleam_module)
}

pub fn print(module: glance.Module) -> String {
  glance_printer.print(module)
}

pub fn custom_type(
  type_: elm.Type,
  public: Bool,
) -> Result(glance.Definition(glance.CustomType), Error) {
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
        |> list.map(variant),
    ),
  ))
}

fn variant(constructor: elm.ValueConstructor) -> glance.Variant {
  let elm.ValueConstructor(elm.TypeName(name), arguments) = constructor
  glance.Variant(
    name,
    arguments
      |> list.map(fn(ann: elm.TypeAnnotation) {
        case ann {
          elm.GenericType(name) ->
            glance.UnlabelledVariantField(glance.VariableType(name))
          elm.Unit ->
            glance.UnlabelledVariantField(glance.NamedType("Nil", None, []))
        }
      }),
  )
}

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
  // It seems to reverse the order when outputting Gleam source code.
  let module =
    glance.Module(..module, custom_types: module.custom_types |> list.reverse)
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
  case arguments {
    [elm.Record(elm.RecordDefinition(fields))] -> record(name, fields)
    _ ->
      glance.Variant(
        name,
        arguments
          |> list.map(type_annotation)
          |> list.map(glance.UnlabelledVariantField),
      )
  }
}

fn record(name: String, fields: List(elm.RecordField)) -> glance.Variant {
  glance.Variant(
    name,
    fields
      |> list.map(record_field),
  )
}

fn record_field(record_field: elm.RecordField) -> glance.VariantField {
  glance.LabelledVariantField(
    type_annotation(record_field.type_),
    record_field.name,
  )
}

fn type_annotation(ann: elm.TypeAnnotation) -> glance.Type {
  case ann {
    elm.GenericType(name) -> glance.VariableType(name)
    elm.Unit -> glance.NamedType("Nil", None, [])
    elm.Tupled(annotations) ->
      glance.TupleType(annotations |> list.map(type_annotation))
    elm.Typed(elm.TypeName(type_name), annotations) ->
      glance.NamedType(
        type_name,
        None,
        annotations |> list.map(type_annotation),
      )
    elm.Record(_record_definition) ->
      panic as "Multiple record type annotations are not supported"
  }
}

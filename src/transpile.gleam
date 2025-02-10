import elm/ast as elm
import eval.{type Eval}
import glance
import glance_printer
import gleam/list
import gleam/option.{None}
import transpile/context.{type Context}

pub type Error {
  Error(glance.Error)
}

pub type Transpiler(a) =
  Eval(a, Error, Context)

pub fn initial_ctx() -> Context {
  context.new()
}

pub fn module(elm_ast: elm.Module) -> Transpiler(glance.Module) {
  use _ <- eval.try(elm_ast.exposing |> context.handle_exposing)
  use custom_types <- eval.try(
    elm_ast.declarations
    |> list.map(fn(decl) {
      let elm.CustomTypeDeclaration(custom_type) = decl
      custom_type
    })
    |> list.map(fn(type_) { custom_type(type_) })
    |> eval.all(),
  )
  glance.Module([], custom_types, [], [], [])
  |> eval.return
}

pub fn run(t: Transpiler(a), s: Context) -> Result(a, Error) {
  eval.run(t, s)
}

pub fn print(module: glance.Module) -> String {
  // It seems to reverse the order when outputting Gleam source code.
  let module =
    glance.Module(..module, custom_types: module.custom_types |> list.reverse)
  glance_printer.print(module)
}

pub fn custom_type(
  type_: elm.Type,
) -> Transpiler(glance.Definition(glance.CustomType)) {
  use _ <- eval.try(context.handle_custom_type(type_))

  let elm.Type(elm.TypeName(name), generics, constructors) = type_
  use context.Visibility(is_public:, is_opaque:) <- eval.try(context.visibility(
    name,
  ))

  glance.Definition(
    [],
    glance.CustomType(
      name,
      case is_public {
        True -> glance.Public
        False -> glance.Private
      },
      is_opaque,
      generics,
      constructors
        |> list.map(variant),
    ),
  )
  |> eval.return
}

fn variant(constructor: elm.ValueConstructor) -> glance.Variant {
  let elm.ValueConstructor(elm.TypeName(name), arguments) = constructor
  case arguments {
    // Currently, generic records are represented as regular
    // records in Gleam.
    // TODO: Emit a warning.
    [elm.Record(def)] | [elm.GenericRecord(_, def)] -> {
      let elm.RecordDefinition(fields) = def
      record(name, fields)
    }
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
    elm.FunctionType(lhs, rhs) -> {
      case flatten_function_type_annotation(rhs) |> list.reverse {
        [] -> glance.FunctionType([], type_annotation(lhs))
        [ret, ..rest] ->
          glance.FunctionType(
            [lhs, ..list.reverse(rest)] |> list.map(type_annotation),
            type_annotation(ret),
          )
      }
    }
    elm.Record(_) | elm.GenericRecord(_, _) ->
      // TODO: Emit warning
      panic as "Multiple record type annotations are not supported"
  }
}

fn flatten_function_type_annotation(
  type_: elm.TypeAnnotation,
) -> List(elm.TypeAnnotation) {
  // Parses a recursive function type annotation into a list of
  // argument types and a return type.

  case type_ {
    elm.FunctionType(arg, rest) -> {
      let args = flatten_function_type_annotation(rest)
      [arg, ..args]
    }
    _ -> [type_]
  }
}

import eval.{type Eval}
import glance
import glance_printer
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import glelm/elm/ast as elm
import glelm/transpile/context.{type Context}

pub type Error {
  TranspileError(glance.Error)
}

pub type Transpiler(a) =
  Eval(a, Error, Context)

pub fn initial_ctx() -> Context {
  context.new()
}

pub fn module(elm_ast: elm.Module) -> Transpiler(glance.Module) {
  use _ <- eval.try(elm_ast.exposing |> context.handle_exposing)
  use #(custom_types, type_aliases) <- eval.try(
    declarations(elm_ast.declarations)
    |> eval.map(partition_declarations),
  )
  glance.Module(
    imports: [],
    custom_types:,
    type_aliases:,
    constants: [],
    functions: [],
  )
  |> eval.return
}

pub fn run(t: Transpiler(a), s: Context) -> Result(a, Error) {
  eval.run(t, s)
}

pub fn print(module: glance.Module) -> String {
  // It seems to reverse the order when outputting Gleam source code.
  let module =
    glance.Module(
      ..module,
      custom_types: module.custom_types |> list.reverse,
      type_aliases: module.type_aliases |> list.reverse,
    )
  glance_printer.print(module)
}

// Used internally to facilitate generation of glance declarations.
type Decl {
  CustomType(glance.Definition(glance.CustomType))
  TypeAlias(glance.Definition(glance.TypeAlias))
}

fn declarations(declarations: List(elm.Declaration)) -> Transpiler(List(Decl)) {
  declarations
  |> list.map(fn(decl) {
    case decl {
      elm.CustomTypeDeclaration(type_) -> custom_type(type_)

      elm.AliasDeclaration(alias) -> type_alias(alias)
    }
  })
  |> eval.all
}

// Glance puts aliases before custom types in the generated output and uses
// separate Definition type for each, unlike Elm AST which preserves their
// original relative positions in the source.
fn partition_declarations(
  declarations: List(Decl),
) -> #(
  List(glance.Definition(glance.CustomType)),
  List(glance.Definition(glance.TypeAlias)),
) {
  declarations
  |> list.map(fn(decl) {
    case decl {
      CustomType(def) -> #(Some(def), None)
      TypeAlias(def) -> #(None, Some(def))
    }
  })
  |> list.unzip
  |> pair.map_first(option.values)
  |> pair.map_second(option.values)
}

fn type_alias(alias: elm.TypeAlias) -> Transpiler(Decl) {
  use _ <- eval.try(context.handle_type_alias(alias))
  let elm.TypeAlias(elm.TypeName(name), generics, ann) = alias

  use context.Visibility(is_public:, is_opaque: _) <- eval.try(
    context.visibility(name),
  )
  case ann {
    elm.GenericType(_) | elm.Unit | elm.Tupled(_) | elm.Typed(_, _) ->
      type_alias_for_aliasable_type(name, is_public, generics, ann)
      |> eval.map(TypeAlias)
    elm.Record(_) | elm.GenericRecord(_, _) | elm.FunctionType(_, _) -> {
      let constructors = [elm.ValueConstructor(alias.name, [ann])]
      type_alias_fallback(name, is_public, is_public, generics, constructors)
      |> eval.map(CustomType)
    }
  }
}

fn type_alias_for_aliasable_type(
  name: String,
  is_public: Bool,
  generics: List(String),
  ann: elm.TypeAnnotation,
) -> Transpiler(glance.Definition(glance.TypeAlias)) {
  glance.Definition(
    [],
    glance.TypeAlias(name, publicity(is_public), generics, type_annotation(ann)),
  )
  |> eval.return
}

fn type_alias_fallback(
  name: String,
  is_public: Bool,
  is_opaque: Bool,
  generics: List(elm.GenericName),
  constructors: List(elm.ValueConstructor),
) {
  glance.Definition(
    [],
    glance.CustomType(
      name,
      publicity(is_public),
      is_opaque,
      generics,
      constructors
        |> list.map(variant),
    ),
  )
  |> eval.return
}

fn custom_type(type_: elm.Type) -> Transpiler(Decl) {
  use _ <- eval.try(context.handle_custom_type(type_))

  let elm.Type(elm.TypeName(name), generics, constructors) = type_
  use context.Visibility(is_public:, is_opaque:) <- eval.try(context.visibility(
    name,
  ))

  glance.Definition(
    [],
    glance.CustomType(
      name,
      publicity(is_public),
      is_opaque,
      generics,
      constructors
        |> list.map(variant),
    ),
  )
  |> eval.return
  |> eval.map(CustomType)
}

fn publicity(is_public: Bool) -> glance.Publicity {
  case is_public {
    True -> glance.Public
    False -> glance.Private
  }
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

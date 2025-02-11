import eval.{type Eval}
import eval/context
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import glelm/elm/ast as elm

pub type Export {
  TypeOrAliasExport(is_opaque: Bool)
  FunctionExport
}

pub type Exports {
  // No declarations are exposed from the module.
  NoPublic
  // All declarations are exposed from the module.
  // Transpiler will add all encountered declarations to the dict.
  // The logic is in handle_declaration.
  AllPublic(dict.Dict(String, Export))
  // Only the declarations in the dict are exposed from the module.
  SomePublic(dict.Dict(String, Export))
}

pub type Context {
  Context(exposing: Exports)
}

pub fn new() -> Context {
  Context(exposing: NoPublic)
}

pub fn handle_exposing(exposing: elm.Exposing) -> Eval(Nil, e, Context) {
  use _ <- context.update()
  let exposing = case exposing {
    elm.ExposingNothing -> NoPublic
    elm.ExposingAll -> AllPublic(dict.new())
    elm.Explicit(list) ->
      SomePublic(list |> list.map(expose_to_export) |> dict.from_list)
  }
  Context(exposing: exposing)
}

fn expose_to_export(expose: elm.TopLevelExpose) -> #(String, Export) {
  case expose {
    elm.TypeOrAliasExpose(elm.TypeName(name), is_opaque) -> #(
      name,
      TypeOrAliasExport(is_opaque),
    )
    elm.FunctionExpose(elm.FunctionName(name)) -> #(name, FunctionExport)
  }
}

// TODO: Merge handle_custom_type and handle_type_alias
pub fn handle_custom_type(type_: elm.Type) -> Eval(Nil, e, Context) {
  use ctx: Context <- context.update()
  let elm.TypeName(name) = type_.name
  let exposing = case ctx.exposing {
    AllPublic(exposing) ->
      AllPublic(exposing |> dict.insert(name, TypeOrAliasExport(False)))
    other -> other
  }
  Context(exposing: exposing)
}

pub fn handle_type_alias(alias: elm.TypeAlias) -> Eval(Nil, e, Context) {
  use ctx: Context <- context.update()
  let elm.TypeName(name) = alias.name
  let exposing = case ctx.exposing {
    AllPublic(exposing) ->
      AllPublic(exposing |> dict.insert(name, TypeOrAliasExport(False)))
    other -> other
  }
  Context(exposing: exposing)
}

pub type Visibility {
  // TODO: is_opaque only applies to types. Figure out
  // how to best represent this.
  Visibility(is_public: Bool, is_opaque: Bool)
}

pub fn visibility(name: String) -> Eval(Visibility, e, Context) {
  use ctx: Context <- eval.try(context.get())
  let visibility =
    case ctx.exposing {
      NoPublic -> None
      AllPublic(_) -> Some(Visibility(is_public: True, is_opaque: False))
      SomePublic(exposing) ->
        exposing
        |> dict.get(name)
        |> option.from_result
        |> option.map(fn(export) {
          case export {
            TypeOrAliasExport(is_opaque) ->
              Visibility(is_public: True, is_opaque:)
            FunctionExport -> Visibility(is_public: True, is_opaque: False)
          }
        })
    }
    |> option.unwrap(Visibility(is_public: False, is_opaque: False))
  eval.return(visibility)
}

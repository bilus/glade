import elm/parser
import glance
import gleam/io
import gleam/result
import transpile

pub type Error(ctx) {
  ElmParserError(parser.Error(ctx))
  TranspileError(transpile.Error)
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
    parser.run(elm_src, parser.module_parser())
    |> result.map_error(ElmParserError),
  )
  io.debug("*** ELM AST")
  io.debug(elm_ast)
  use gleam_ast <- result.try(
    transpile.module(elm_ast) |> result.map_error(TranspileError),
  )
  io.debug("*** GLEAM AST")
  io.debug(gleam_ast)
  let gleam_src = transpile.print(gleam_ast)
  io.debug("*** GLEAM SRC")
  io.debug(gleam_src)
  use gleam_ast2 <- result.try(
    glance.module(gleam_src) |> result.map_error(GlanceError),
  )
  io.debug("*** GLEAM AST 2")
  io.debug(gleam_ast2)

  Ok("")
}

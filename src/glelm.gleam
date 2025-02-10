import debug
import elm/lexer
import elm/parser
import glance
import gleam/io
import gleam/result
import nibble
import transpile

pub type RuntimeError(ctx) {
  ParserError(parser.Error(ctx))
  TranspileError(transpile.Error)
  GlanceError(glance.Error)
  InternalError(String)
}

pub fn run() -> Result(String, RuntimeError(a)) {
  let elm_src =
    "
module Exposing exposing (..)

type PublicType = PublicType

-- TODO: Add function here.
"
  io.println(elm_src)
  use elm_ast <- result.try(
    parser.run(elm_src, parser.module())
    |> result.map_error(ParserError),
  )
  debug.log("*** ELM AST")
  debug.log(elm_ast)
  use gleam_ast <- result.try(
    transpile.module(elm_ast) |> result.map_error(TranspileError),
  )
  debug.log("*** GLEAM AST")
  debug.log(gleam_ast)
  let gleam_src = transpile.print(gleam_ast)
  debug.log("*** GLEAM SRC")
  io.print(gleam_src)
  use gleam_ast2 <- result.try(
    glance.module(gleam_src) |> result.map_error(GlanceError),
  )
  debug.log("*** GLEAM AST 2")
  debug.log(gleam_ast2)

  Ok(gleam_src)
}

pub fn main() {
  // let _ = print_glance_output()

  case run() {
    Ok(_) -> io.println("Success!")
    Error(TranspileError(_)) -> io.println("Transpile error")
    Error(ParserError(parser.LexerError(_))) -> {
      io.println("Lexer error")
      // debug.log(e)
    }
    Error(ParserError(parser.ParserError([dead_end, ..]))) -> {
      io.println("Parser error")
      let de: nibble.DeadEnd(lexer.Token, Nil) = dead_end
      io.debug(de)
      Nil
    }
    Error(GlanceError(_)) -> io.println("Glance error")
    Error(InternalError(msg)) -> io.println("Internal error: " <> msg)
    _ -> io.println("Unknown error")
  }
}

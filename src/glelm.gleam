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

pub fn run() {
  io.debug("main")
  let elm_src =
    "
type Maybe a
  = Just a
  | Nothing
"
  use elm_ast <- result.try(
    parser.run(elm_src, parser.module_parser())
    |> result.map_error(ParserError),
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

pub fn main() {
  case run() {
    Ok(_) -> io.debug("Success")
    Error(TranspileError(_)) -> io.debug("Transpile error")
    Error(ParserError(parser.LexerError(_))) -> {
      io.debug("Lexer error")
      // io.debug(e)
    }
    Error(ParserError(parser.ParserError([dead_end, ..]))) -> {
      io.debug("Parser error")
      let de: nibble.DeadEnd(lexer.Token, Nil) = dead_end
      io.debug(de)
      io.debug("")
    }
    Error(GlanceError(_)) -> io.debug("Glance error")
    Error(InternalError(msg)) -> io.debug("Internal error: " <> msg)
    _ -> io.debug("Unknown error")
  }
}

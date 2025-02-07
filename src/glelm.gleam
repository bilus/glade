import debug
import elm/lexer
import elm/parser
import glance
import glance_printer
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
  let elm_src =
    "
type Maybe a
  = Just a
  | Nothing

type Maybe2 a = Just2 a | Nothing2
"
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
  debug.log(gleam_src)
  use gleam_ast2 <- result.try(
    glance.module(gleam_src) |> result.map_error(GlanceError),
  )
  debug.log("*** GLEAM AST 2")
  debug.log(gleam_ast2)

  Ok("")
}

pub fn main() {
  let _ = print_glance_output()

  case run() {
    Ok(_) -> debug.log("Success")
    Error(TranspileError(_)) -> debug.log("Transpile error")
    Error(ParserError(parser.LexerError(_))) -> {
      debug.log("Lexer error")
      // debug.log(e)
    }
    Error(ParserError(parser.ParserError([dead_end, ..]))) -> {
      debug.log("Parser error")
      let de: nibble.DeadEnd(lexer.Token, Nil) = dead_end
      debug.log(de)
      debug.log("")
    }
    Error(GlanceError(_)) -> debug.log("Glance error")
    Error(InternalError(msg)) -> debug.log("Internal error: " <> msg)
    _ -> debug.log("Unknown error")
  }
}

fn print_glance_output() {
  let gleam =
    "
pub type Foo(a, b) {
  Foo(#(a))
  Bar(#(a, b))
}
"
  use ast <- result.try(glance.module(gleam))
  debug.log(ast)
  let source = glance_printer.print(ast)
  debug.log(source)
  Ok(Nil)
}

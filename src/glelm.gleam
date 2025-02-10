import argv
import glance
import gleam/int
import gleam/io
import gleam/result
import glelm/debug
import glelm/elm/lexer
import glelm/elm/parser
import glelm/transpile
import nibble
import simplifile as file

pub type RuntimeError(ctx) {
  ArgumentError(String)
  ParserError(parser.Error(ctx))
  TranspileError(transpile.Error)
  GlanceError(glance.Error)
  InternalError(String)
}

pub fn run() -> Result(String, RuntimeError(a)) {
  use elm_src <- result.try(case argv.load().arguments {
    [elm_file] ->
      file.read(elm_file)
      |> result.map_error(fn(e) { ArgumentError(file.describe_error(e)) })
    _ -> Error(ArgumentError("Missing path to an .elm source file"))
  })
  use elm_ast <- result.try(
    parser.parse(elm_src, parser.module())
    |> result.map_error(ParserError),
  )
  debug.log("*** ELM AST")
  debug.log(elm_ast)
  use gleam_ast <- result.try(
    transpile.module(elm_ast)
    |> transpile.run(transpile.initial_ctx())
    |> result.map_error(TranspileError),
  )
  debug.log("*** GLEAM AST")
  debug.log(gleam_ast)
  let gleam_src = transpile.print(gleam_ast)
  debug.log("*** GLEAM SRC")
  io.print(gleam_src)
  Ok(gleam_src)
}

pub fn main() {
  // let _ = print_glance_output()

  case run() {
    Ok(_) -> io.println("Success!")
    Error(ArgumentError(msg)) -> io.println(msg)
    Error(TranspileError(_)) -> io.println("Transpile error")
    Error(ParserError(parser.NoMatchingTokenError(row, col, lexeme))) -> {
      io.println(
        "No such token "
        <> lexeme
        <> " at "
        <> int.to_string(row)
        <> ":"
        <> int.to_string(col),
      )
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

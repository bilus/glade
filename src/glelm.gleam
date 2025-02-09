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

pub fn run() {
  let elm_src =
    "
type FooBar a
    = Bar (Result String a)
    | Baz (Int, Int)
    | Qux { str: String, int: a }
"
  io.print(elm_src)
  io.println("")
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

  Ok("")
}

pub fn main() {
  // let _ = print_glance_output()

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
// fn print_glance_output() {
//   let gleam =
//     "
// pub type Foo(a) {
//   Foo(field: String, bar: a)
//   Baz(String)
// }
// "
//   use ast <- result.try(glance.module(gleam))
//   io.debug(ast)
//   let source = glance_printer.print(ast)
//   io.print(source)
//   Ok(Nil)
// }
// fn calculator() {
//   use ast <- result.try(
//     lexer.new()
//     |> lexer.run("1 * 2 + 3")
//     |> result.map(nibble.run(_, expression())),
//   )
//   Ok(ast)
// }

// type Expression {
//   Addition(Expression, Expression)
//   Multiplication(Expression, Expression)
//   Number(Float)
// }

// fn term() {
//   nibble.one_of([number(), sum()])
// }

// fn sum() {
//   use lhs <- nibble.do(expression())
//   use _ <- nibble.do(nibble.token(lexer.Plus))
//   use rhs <- nibble.do(expression())
//   nibble.succeed(Addition(lhs, rhs))
// }

// fn number() {
//   nibble.take_map("Expected float or int literal", fn(tok) {
//     case tok {
//       lexer.FloatLiteral(f) -> Some(Number(f))
//       lexer.IntLiteral(i) -> Some(Number(int.to_float(i)))
//       _ -> None
//     }
//   })
// }

// fn expression() {
//   nibble.one_of([sub_expression(), number()])
// }

// fn sub_expression() {
//   use num <- nibble.do(number())
//   use expr <- nibble.do({
//     use op <- nibble.take_map("Expected operator")
//     case op {
//       lexer.Plus -> Some(Addition)
//       lexer.Multiply -> Some(Multiplication)
//       _ -> None
//     }
//   })
//   use rhs <- nibble.do(expression())
//   nibble.succeed(expr(num, rhs))
// }

// pub fn main() {
//   use ast <- result.try(calculator())
//   io.debug(ast)
//   Ok("")
// }

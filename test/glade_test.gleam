import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/set
import gleeunit
import gleeunit/should
import nibble/lexer
// type Token {
//   Module
//   Identifier(String)
//   Exposing
//   LParen
//   RParen
//   Ellipsis
// }

// fn new_lexer() -> lexer.Lexer(Token, Nil) {
//   lexer.simple([
//     lexer.keyword("module", "\\s", Module),
//     lexer.keyword("exposing", "\\s", Exposing),
//     lexer.identifier("[a-zA-Z]", "[a-zA-Z0-9_]", set.new(), Identifier),
//     lexer.token("(", LParen),
//     lexer.token(")", RParen),
//     lexer.token("...", Ellipsis),
//     // Skip over whitespace, we don't care about it!
//     lexer.whitespace(Nil)
//       |> lexer.ignore,
//   ])
// }

// type ModuleName {
//   ModuleName(String)
// }

// type Name {
//   Name(String)
// }

// type ExportSet {
//   AllExport
//   SubsetExport(List(ExportSet))
//   FunctionExport(Name)
//   TypeExport(Name, Option(ExportSet))
// }

// type Statement {
//   ModuleDeclaration(module_name: ModuleName, export_set: ExportSet)
// }

// pub fn main() {
//   gleeunit.main()
// }

// // gleeunit test functions end in `_test`
// pub fn elm_test() {
//   "module Abc exposing (...)"
//   |> lexer.run(new_lexer())
//   |> result.map(fn(result) {
//     result
//     |> list.map(token)
//   })
//   |> should.equal(
//     Ok([Module, Identifier("Abc"), Exposing, LParen, Ellipsis, RParen]),
//   )
// }

// fn token(lexer_token: lexer.Token(Token)) -> Token {
//   let lexer.Token(value:, ..) = lexer_token
//   value
// }

// type EToken {
//   EInt(Int)
//   EFloat(Float)
// }

// fn new_expression_lexer() -> lexer.Lexer(EToken, Nil) {
//   lexer.simple([
//     lexer.number(EInt, EFloat),
//     lexer.whitespace(Nil) |> lexer.ignore,
//   ])
// }

// fn parse_expression(
//   tokens: List(lexer.Token(EToken)),
// ) -> Result(EToken, lexer.Error) {
//   Ok(EToken.EInt(1))
// }

// fn eval_expression(
//   tokens: List(lexer.Token(EToken)),
// ) -> Result(Float, lexer.Error) {
//   Ok(0.0)
// }

// pub fn expression_test() {
//   "1"
//   |> lexer.run(new_expression_lexer())
//   |> result.then(eval_expression)
//   |> should.equal(Ok(1.0))
// }

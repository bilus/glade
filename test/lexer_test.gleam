import elm/lexer
import gleam/list
import gleeunit/should

type TestCase {
  TestCase(name: String, elm_source: String, expected_tokens: List(lexer.Token))
}

pub fn lexer_test() {
  let test_cases = [
    // TestCase("empty source produces no tokens", "", []),
    // TestCase("single line", "type Maybe a = Just a | Nothing", [
    //   lexer.TypeKeyword,
    //   lexer.LayoutStart,
    //   lexer.TypeName("Maybe"),
    //   lexer.Identifier("a"),
    //   lexer.Eq,
    //   lexer.TypeName("Just"),
    //   lexer.Identifier("a"),
    //   lexer.VerticalBar,
    //   lexer.TypeName("Nothing"),
    //   lexer.LayoutEnd,
    // ]),
    TestCase(
      "multiple types",
      "
type Either a b = Left a | Right b

type Maybe a
     = Just a
     | Nothing",
      [
        // Either
        lexer.TypeKeyword,
        lexer.LayoutStart,
        lexer.TypeName("Either"),
        lexer.Identifier("a"),
        lexer.Identifier("b"),
        lexer.Eq,
        lexer.TypeName("Left"),
        lexer.Identifier("a"),
        lexer.VerticalBar,
        lexer.TypeName("Right"),
        lexer.Identifier("b"),
        lexer.LayoutEnd,
        // Maybe
        lexer.TypeKeyword,
        lexer.LayoutStart,
        lexer.TypeName("Maybe"),
        lexer.Identifier("a"),
        lexer.Eq,
        lexer.TypeName("Just"),
        lexer.Identifier("a"),
        lexer.VerticalBar,
        lexer.TypeName("Nothing"),
        lexer.LayoutEnd,
      ],
    ),
  ]
  test_cases
  |> list.each(run)
  Ok(Nil)
}

fn run(test_case: TestCase) {
  tokens(test_case.elm_source)
  |> should.equal(test_case.expected_tokens)
}

fn tokens(elm_source: String) -> List(lexer.Token) {
  let assert Ok(tokens) = lexer.new() |> lexer.run(elm_source)
  tokens
  |> list.map(fn(token) { token.value })
}

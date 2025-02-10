import gleam/io
import gleam/result
import nibble
import nibble/lexer

// "Pantry: Transforming Nibble's stateless parsers into well-fed stateful operations" ++
//
// or
//
// Pantry for Nibble parsers, keeping track of what's been nibbled. A
// state management extension for Nibble parsers, enabling stateful parsing
// operations.
pub type StatefulParser(a, tok, ctx, s) {
  StatefulParser(run: fn(s) -> nibble.Parser(#(a, s), tok, ctx))
}

pub fn parse(
  tokens: List(lexer.Token(tok)),
  stateful_parser: StatefulParser(a, tok, ctx, s),
  initial_ctx: s,
) -> Result(a, List(nibble.DeadEnd(tok, ctx))) {
  let parser = run(stateful_parser, initial_ctx)
  nibble.run(tokens, parser)
  |> result.map(fn(sa) { sa.0 })
}

pub fn inspect() -> StatefulParser(s, tok, ctx, s) {
  StatefulParser(run: fn(state) {
    io.debug(state)
    nibble.return(#(state, state))
  })
}

pub fn lift(
  parser: nibble.Parser(a, tok, ctx),
) -> StatefulParser(a, tok, ctx, s) {
  StatefulParser(run: fn(state) { nibble.map(parser, fn(a) { #(a, state) }) })
}

pub fn run(
  parser: StatefulParser(a, tok, ctx, s),
  state: s,
) -> nibble.Parser(#(a, s), tok, ctx) {
  parser.run(state)
}

pub fn do(
  stateful_parser: StatefulParser(a, tok, ctx, s),
  f: fn(a) -> StatefulParser(b, tok, ctx, s),
) -> StatefulParser(b, tok, ctx, s) {
  // state.State(run: state.do(stateful_parser.run, f(parser).run))
  let g: fn(s) -> nibble.Parser(#(b, s), tok, ctx) = fn(state) {
    let parser = stateful_parser.run(state)
    use #(value, state) <- nibble.do(parser)
    run(f(value), state)
  }
  StatefulParser(run: g)
}

pub fn get() -> StatefulParser(s, tok, ctx, s) {
  StatefulParser(run: fn(state) { nibble.return(#(state, state)) })
}

pub fn modify(f: fn(s) -> s) -> StatefulParser(s, tok, ctx, s) {
  StatefulParser(run: fn(state) {
    let new_state = f(state)
    nibble.return(#(new_state, new_state))
  })
}

pub fn put(state: s) -> StatefulParser(Nil, tok, ctx, s) {
  StatefulParser(run: fn(_) { nibble.return(#(Nil, state)) })
}

pub fn return(a: a) -> StatefulParser(a, tok, ctx, s) {
  StatefulParser(run: fn(state) { nibble.return(#(a, state)) })
}

# glelm

Elm-to-Gleam transpiler.

WORK-IN-PROGRESS.

Currently supported:
* Naive transpilling of type declarations (e.g. `Maybe` is `Maybe` in generated Gleam code).
* Sum types
* Typed Records
* Function types
* Tuples
* Unit

Coming up soon:
* Type aliases `type alias A = B`
* Untyped records  `type alias R = { foo: Int }`

Unsupported:
* Function declarations
* Ports


[![Package Version](https://img.shields.io/hexpm/v/glelm)](https://hex.pm/packages/glelm)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glelm/)

```sh
cat samples/Sample1.elm
```

```elm
module Sample1 exposing (Declaration(..), Node)


-- Elm declaration
module Sample1 exposing (Declaration(..), Node)

{-| Elm declaration
-}
type Declaration
    = FunctionDeclaration Function
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration Signature
    | InfixDeclaration Infix
    | Destructuring
        { pattern : Node Pattern
        , expression : Node Expression
        }


{-| Provides additional context
-}
type Node a
    = Node Range a
```

``` sh
gleam run samples/Sample1.elm
[...]
```

```gleam
pub type Declaration {
  FunctionDeclaration(Function)
  AliasDeclaration(TypeAlias)
  CustomTypeDeclaration(Type)
  PortDeclaration(Signature)
  InfixDeclaration(Infix)
  Destructuring(pattern: Node(Pattern), expression: Node(Expression))
}

pub opaque type Node(a) {
  Node(Range, a)
}
Success!
```

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

### Running elm-syntax

There is a web app you can use to turn Elm source code into AST compatible with
[elm-syntax](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.3.8/)
which is an inspiration for Elm AST in this project. The generated JSON isn't
prefect but it's close enough to help figuring out some quirkier aspects.

``` sh
cd tools/elm-syntax
elm reactor
```

### Contributing: supporting more syntax

The flow for adding support for more Elm syntax is as follows:

1. Add a test case to `test/transpile` by adding two files, `xyz.elm` containing
   the input and `xyz.expected`.
2. If necessary, use `tools/elm-syntax` to inspect the corresponding elm-syntax
   AST.
3. If you need to add new tokens, add them in `elm/lexer.gleam`. Don't forget
   about tests.
4. Implement the parser in `elm/parser.gleam`, adding tests where necessary.
5. Update the transpiler in `transpile.gleam`.
6. Run tests via `gleam test`. Iterate until they pass.

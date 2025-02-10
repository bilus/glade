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

import elm/lexer
import gleam/io
import gleam/option.{None, Some}
import gleam/result
import nibble.{do, return}

type Name =
  String

type Generic =
  String

type Type {
  Type(name: Name, generics: List(Generic), constructor: List(ValueConstructor))
}

type ValueConstructor {
  ValueConstructor(name: Name, arguments: List(TypeAnnotation))
}

// type TypeAnnotation
//     = GenericType String
//     | Typed
//           (Node ( ModuleName, String ))
//           (List (Node TypeAnnotation))
//     | Unit
//     | Tupled (List (Node TypeAnnotation))
//     | Record RecordDefinition
//     | GenericRecord (Node String) (Node RecordDefinition)
//     | FunctionTypeAnnotation
//           (Node TypeAnnotation)
//           (Node TypeAnnotation)
// Custom type for different type annotations. For example:

// GenericType: a
// Typed: Maybe (Int -> String)
// Unit: ()
// Tuples: (a, b, c)
// Record: { name : String}
// GenericRecord: { a | name : String}
// FunctionTypeAnnotation: Int -> String

type TypeAnnotation {
  GenericType(Generic)
}

pub fn main() {
  let tokens =
    lexer.new()
    |> lexer.run(
      "
type Maybe a
  = Just a
  | Nothing
",
    )
  // |> result.try
  io.debug(tokens)
}

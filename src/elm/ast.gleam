pub type Module {
  Module(declarations: List(Declaration))
}

pub type Declaration {
  CustomTypeDeclaration(Type)
}

pub type TypeName {
  TypeName(name: String)
}

pub type GenericName =
  String

pub type Type {
  Type(
    name: TypeName,
    generics: List(GenericName),
    constructor: List(ValueConstructor),
  )
}

pub type ValueConstructor {
  ValueConstructor(name: TypeName, arguments: List(TypeAnnotation))
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

pub type TypeAnnotation {
  GenericType(GenericName)
  Unit
  Tupled(List(TypeAnnotation))
  Typed(TypeName, List(TypeAnnotation))
}

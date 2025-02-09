pub type Module {
  Module(name: String, declarations: List(Declaration))
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

/// Custom type for different type annotations.
///
/// For example:
///
///   GenericType: a
///   Typed: Maybe (Int -> String)
///   Unit: ()
///   Tuples: (a, b, c)
///   Record: { name : String}
///   GenericRecord: { a | name : String}
///   FunctionType: Int -> String
pub type TypeAnnotation {
  GenericType(GenericName)
  Unit
  Tupled(List(TypeAnnotation))
  Typed(TypeName, List(TypeAnnotation))
  Record(RecordDefinition)
  GenericRecord(GenericName, RecordDefinition)
  FunctionType(TypeAnnotation, TypeAnnotation)
}

pub type RecordDefinition {
  RecordDefinition(fields: List(RecordField))
}

pub type RecordFieldName =
  String

pub type RecordField {
  RecordField(name: RecordFieldName, type_: TypeAnnotation)
}

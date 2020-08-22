type t =
  | IntType
  | FltType
  | BoolType
  | StrType
  | LocType
  | ListType
  | TypeType
  | TupleType
  | NullType
  | SymbolType

let str (v : t) : string = match v with
  | IntType    -> "Int"
  | FltType    -> "Flt"
  | BoolType   -> "Bool"
  | StrType    -> "Str"
  | LocType    -> "Obj"
  | ListType   -> "List"
  | TypeType   -> "Type"
  | TupleType  -> "Tuple"
  | NullType   -> "Null"
  | SymbolType -> "Symbol"

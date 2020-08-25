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
  | IntType    -> "__$Int"
  | FltType    -> "__$Flt"
  | BoolType   -> "__$Bool"
  | StrType    -> "__$Str"
  | LocType    -> "__$Obj"
  | ListType   -> "__$List"
  | TypeType   -> "__$Type"
  | TupleType  -> "__$Tuple"
  | NullType   -> "__$Null"
  | SymbolType -> "__$Symbol"

type t =
  | NullType
  | IntType
  | FltType
  | StrType
  | BoolType
  | SymbolType
  | LocType
  | ArrayType
  | ListType
  | TupleType
  | TypeType
  | CurryType

let equal (t1 : t) (t2 : t) : bool = t1 = t2 [@@inline]

let str (v : t) : string =
  match v with
  | NullType -> "__$Null"
  | IntType -> "__$Int"
  | FltType -> "__$Flt"
  | StrType -> "__$Str"
  | BoolType -> "__$Bool"
  | SymbolType -> "__$Symbol"
  | LocType -> "__$Obj"
  | ArrayType -> "__$Array"
  | ListType -> "__$List"
  | TupleType -> "__$Tuple"
  | TypeType -> "__$Type"
  | CurryType -> "__$Curry"

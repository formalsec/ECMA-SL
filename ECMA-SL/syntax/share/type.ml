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

let ( = ) t1 t2 =
  match (t1, t2) with
  | (NullType, NullType)
  | (IntType, IntType)
  | (FltType, FltType)
  | (StrType, StrType)
  | (BoolType, BoolType)
  | (SymbolType, SymbolType)
  | (LocType, LocType)
  | (ArrayType, ArrayType)
  | (ListType, ListType)
  | (TupleType, TupleType)
  | (TypeType, TypeType)
  | (CurryType, CurryType) ->
    true
  | _ -> false

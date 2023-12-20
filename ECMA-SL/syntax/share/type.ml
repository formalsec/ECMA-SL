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
  | CurryType
  | ArrayType

let str (v : t) : string =
  match v with
  | IntType -> "__$Int"
  | FltType -> "__$Flt"
  | BoolType -> "__$Bool"
  | StrType -> "__$Str"
  | LocType -> "__$Obj"
  | ListType -> "__$List"
  | TypeType -> "__$Type"
  | TupleType -> "__$Tuple"
  | NullType -> "__$Null"
  | SymbolType -> "__$Symbol"
  | CurryType -> "__$Curry"
  | ArrayType -> "__$Array"

let ( = ) t1 t2 =
  match (t1, t2) with
  | (IntType, IntType)
  | (FltType, FltType)
  | (BoolType, BoolType)
  | (StrType, StrType)
  | (LocType, LocType)
  | (ListType, ListType)
  | (TypeType, TypeType)
  | (TupleType, TupleType)
  | (NullType, NullType)
  | (SymbolType, SymbolType)
  | (CurryType, CurryType)
  | (ArrayType, ArrayType) ->
    true
  | _ -> false

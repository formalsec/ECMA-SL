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

let pp_string = Format.pp_print_string

let pp fmt = function
  | IntType -> pp_string fmt "__$Int"
  | FltType -> pp_string fmt "__$Flt"
  | BoolType -> pp_string fmt "__$Bool"
  | StrType -> pp_string fmt "__$Str"
  | LocType -> pp_string fmt "__$Obj"
  | ListType -> pp_string fmt "__$List"
  | TypeType -> pp_string fmt "__$Type"
  | TupleType -> pp_string fmt "__$Tuple"
  | NullType -> pp_string fmt "__$Null"
  | SymbolType -> pp_string fmt "__$Symbol"
  | CurryType -> pp_string fmt "__$Curry"
  | ArrayType -> pp_string fmt "__$Array"

let str v = Format.asprintf "%a" pp v

let ( = ) t1 t2 =
  match (t1, t2) with
  | IntType, IntType
  | FltType, FltType
  | BoolType, BoolType
  | StrType, StrType
  | LocType, LocType
  | ListType, ListType
  | TypeType, TypeType
  | TupleType, TupleType
  | NullType, NullType
  | SymbolType, SymbolType
  | CurryType, CurryType
  | ArrayType, ArrayType ->
    true
  | _ -> false

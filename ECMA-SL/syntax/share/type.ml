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

let equal (t1 : t) (t2 : t) : bool = t1 = t2

let pp (fmt : Format.formatter) (t : t) : unit =
  let open Format in
  match t with
  | NullType -> pp_print_string fmt "__$Null"
  | IntType -> pp_print_string fmt "__$Int"
  | FltType -> pp_print_string fmt "__$Flt"
  | StrType -> pp_print_string fmt "__$Str"
  | BoolType -> pp_print_string fmt "__$Bool"
  | SymbolType -> pp_print_string fmt "__$Symbol"
  | LocType -> pp_print_string fmt "__$Obj"
  | ArrayType -> pp_print_string fmt "__$Array"
  | ListType -> pp_print_string fmt "__$List"
  | TupleType -> pp_print_string fmt "__$Tuple"
  | TypeType -> pp_print_string fmt "__$Type"
  | CurryType -> pp_print_string fmt "__$Curry"

let str (t : t) : string = Format.asprintf "%a" pp t

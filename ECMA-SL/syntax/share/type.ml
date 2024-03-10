open EslCore

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

let pp (fmt : Fmt.t) (t : t) : unit =
  let open Fmt in
  match t with
  | NullType -> pp_str fmt "__$Null"
  | IntType -> pp_str fmt "__$Int"
  | FltType -> pp_str fmt "__$Flt"
  | StrType -> pp_str fmt "__$Str"
  | BoolType -> pp_str fmt "__$Bool"
  | SymbolType -> pp_str fmt "__$Symbol"
  | LocType -> pp_str fmt "__$Obj"
  | ArrayType -> pp_str fmt "__$Array"
  | ListType -> pp_str fmt "__$List"
  | TupleType -> pp_str fmt "__$Tuple"
  | TypeType -> pp_str fmt "__$Type"
  | CurryType -> pp_str fmt "__$Curry"

let str (t : t) : string = Fmt.asprintf "%a" pp t

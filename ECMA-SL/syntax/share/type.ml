open EslBase

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

let pp (ppf : Fmt.t) (t : t) : unit =
  let open Fmt in
  match t with
  | NullType -> pp_str ppf "__$Null"
  | IntType -> pp_str ppf "__$Int"
  | FltType -> pp_str ppf "__$Flt"
  | StrType -> pp_str ppf "__$Str"
  | BoolType -> pp_str ppf "__$Bool"
  | SymbolType -> pp_str ppf "__$Symbol"
  | LocType -> pp_str ppf "__$Obj"
  | ArrayType -> pp_str ppf "__$Array"
  | ListType -> pp_str ppf "__$List"
  | TupleType -> pp_str ppf "__$Tuple"
  | TypeType -> pp_str ppf "__$Type"
  | CurryType -> pp_str ppf "__$Curry"

let str (t : t) : string = Fmt.str "%a" pp t

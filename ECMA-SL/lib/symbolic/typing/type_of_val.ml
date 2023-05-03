open Val

let type_of_val (v : Val.t) : Type.t option =
  match v with
  | Void -> None
  | Null -> Some Type.NullType
  | Int _ -> Some Type.IntType
  | Flt _ -> Some Type.FltType
  | Bool _ -> Some Type.BoolType
  | Str _ -> Some Type.StrType
  | Loc _ -> Some Type.LocType
  | List _ -> Some Type.ListType
  | Arr _ -> Some Type.ArrayType
  | Tuple _ -> Some Type.TupleType
  | Curry _ -> Some Type.CurryType
  | Byte _ -> Some Type.IntType
  | Type _ -> Some Type.TypeType
  | Symbol _ -> Some Type.SymbolType

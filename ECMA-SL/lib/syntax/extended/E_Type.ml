type t =
  | AnyType
  | UnknownType
  | NeverType
  | UndefinedType
  | NullType
  | NumberType
  | StringType
  | BooleanType
  | SymbolType
  | LiteralType of Val.t
  | ListType of t
  | TupleType of t list
  | UnionType of t list
  | ObjectType of obj_t
  | RuntimeType of Type.t
  | UserDefinedType of string

and obj_t = { flds : (string, field_t) Hashtbl.t; smry : t option }
and field_t = { t : t; status : status_t }
and status_t = Required | Optional

module Field = struct
  type ft = NamedField of string * (t * bool) | SumryField of t
end

let parse_literal_type (v : Val.t) : t =
  match v with
  | Val.Int _ -> LiteralType v
  | Val.Flt _ -> LiteralType v
  | Val.Str _ -> LiteralType v
  | Val.Bool _ -> LiteralType v
  | Val.Symbol "undefined" -> UndefinedType
  | Val.Symbol _ -> LiteralType v
  | Val.Null -> NullType
  | Val.List [] -> LiteralType v
  | _ -> invalid_arg ("Invalid value '" ^ Val.str v ^ "' for literal type.")

let parse_obj_type (field_lst : Field.ft list) : obj_t =
  let fld_split_fun fld (nflds, sflds) =
    match fld with
    | Field.NamedField (fn, ft) -> ((fn, ft) :: nflds, sflds)
    | Field.SumryField t -> (nflds, t :: sflds)
  in
  let nfld_add_fun flds (fn, (t, opt)) =
    let status = if opt then Optional else Required in
    match Hashtbl.find_opt flds fn with
    | None -> Hashtbl.replace flds fn { t; status }
    | Some _ -> invalid_arg ("Field '" ^ fn ^ "' already in the object.")
  in
  let nflds, sflds = List.fold_right fld_split_fun field_lst ([], []) in
  let flds = Hashtbl.create !Config.default_hashtbl_sz in
  let _ = List.iter (nfld_add_fun flds) nflds in
  match sflds with
  | [] -> { flds; smry = None }
  | t :: [] -> { flds; smry = Some t }
  | _ -> invalid_arg "Duplicated summary field in the object."

let merge_tuple_type (t1 : t) (t2 : t) : t =
  match t1 with
  | TupleType ts -> TupleType (List.append ts [ t2 ])
  | _ -> TupleType [ t1; t2 ]

let rec merge_union_type (t1 : t) (t2 : t) : t =
  match (t1, t2) with
  | _, UnionType ts -> List.fold_right (fun t r -> merge_union_type r t) ts t1
  | UnionType ts, _ ->
      UnionType (if List.mem t2 ts then ts else List.append ts [ t2 ])
  | _ -> if t1 = t2 then t1 else UnionType [ t1; t2 ]

let merge_type (merge_fun : t -> t -> t) (ts : t list) : t =
  let tempty = (NeverType, []) in
  let ts_f, ts_r = match ts with [] -> tempty | f :: r -> (f, r) in
  List.fold_left merge_fun ts_f ts_r

let rec unfold_type (addNonLiteral : bool) (t : t) : t list =
  match (t, addNonLiteral) with
  | UndefinedType, _ -> [ t ]
  | NullType, _ -> [ t ]
  | LiteralType v, _ -> [ t ]
  | BooleanType, _ ->
      [ LiteralType (Val.Bool true); LiteralType (Val.Bool false) ]
  | UnionType ts, _ -> List.concat (List.map (unfold_type addNonLiteral) ts)
  | _, true -> [ t ]
  | _, false -> []

let fold_type (ts : t list) =
  let _fold_bool_f t =
    match t with LiteralType (Val.Bool _) -> false | _ -> true
  in
  let hasBoolTrue = List.mem (LiteralType (Val.Bool true)) ts in
  let hasBoolFalse = List.mem (LiteralType (Val.Bool false)) ts in
  if (not hasBoolTrue) || not hasBoolFalse then ts
  else BooleanType :: List.filter _fold_bool_f ts

let ft_optional (ft : field_t) : bool =
  match ft.status with Required -> false | Optional -> true

let get_tfld (ft : field_t) : t =
  match ft.status with
  | Optional -> merge_union_type ft.t UndefinedType
  | _ -> ft.t

let get_obj_fld_list (tobj : obj_t) : (string * field_t) list =
  let nflds = Hashtbl.fold (fun fn ft r -> (fn, ft) :: r) tobj.flds [] in
  let sfld =
    match tobj.smry with
    | Some smry' -> [ ("*", { t = smry'; status = Required }) ]
    | None -> []
  in
  List.append nflds sfld

let rec str (t : t) : string =
  match t with
  | AnyType -> "any"
  | UnknownType -> "unknown"
  | NeverType -> "never"
  | UndefinedType -> "undefined"
  | NullType -> "null"
  | NumberType -> "number"
  | StringType -> "string"
  | BooleanType -> "boolean"
  | SymbolType -> "symbol"
  | LiteralType v -> Val.str v
  | ListType t' -> "[" ^ str t' ^ "]"
  | TupleType ts ->
      "(" ^ String.concat " * " (List.map (fun el -> str el) ts) ^ ")"
  | UnionType ts ->
      "(" ^ String.concat " | " (List.map (fun el -> str el) ts) ^ ")"
  | ObjectType t' ->
      let field_opt_str ft = if ft_optional ft then "?" else "" in
      let field_str (fn, ft) = fn ^ field_opt_str ft ^ ": " ^ str ft.t in
      let fields = get_obj_fld_list t' in
      "{ " ^ String.concat ", " (List.map (fun f -> field_str f) fields) ^ " }"
  | RuntimeType t' -> "runtime(" ^ Type.str t' ^ ")"
  | UserDefinedType t' -> t'

let type_widening (t : t) : t =
  match t with
  | LiteralType Val.Null -> NullType
  | LiteralType (Val.Int _) -> NumberType
  | LiteralType (Val.Flt _) -> NumberType
  | LiteralType (Val.Str _) -> StringType
  | LiteralType (Val.Bool _) -> BooleanType
  | LiteralType (Val.Symbol "undefined") -> UndefinedType
  | LiteralType (Val.Symbol _) -> SymbolType
  | LiteralType (Val.Type t') -> RuntimeType t'
  | _ -> t

let to_runtime (t : t) : t =
  match t with
  | UndefinedType -> RuntimeType Type.SymbolType
  | NullType -> RuntimeType Type.NullType
  | NumberType -> RuntimeType Type.TypeType
  | StringType -> RuntimeType Type.StrType
  | BooleanType -> RuntimeType Type.BoolType
  | SymbolType -> RuntimeType Type.SymbolType
  | LiteralType (Val.Int _) -> RuntimeType Type.IntType
  | LiteralType (Val.Flt _) -> RuntimeType Type.FltType
  | LiteralType Val.Null -> RuntimeType Type.NullType
  | LiteralType (Val.Str _) -> RuntimeType Type.StrType
  | LiteralType (Val.Bool _) -> RuntimeType Type.BoolType
  | LiteralType (Val.Symbol _) -> RuntimeType Type.SymbolType
  | LiteralType (Val.Type t') -> RuntimeType t'
  | ObjectType _ -> RuntimeType Type.LocType
  | RuntimeType _ -> t
  | _ -> RuntimeType Type.TypeType

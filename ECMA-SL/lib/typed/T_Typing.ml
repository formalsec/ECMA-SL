open E_Type

let literal_terr (expr : E_Expr.t) (texpr : t) : t =
  match expr with E_Expr.Val _ -> E_Type.type_widening texpr | _ -> texpr

let check_literal_narrowing (expr : E_Expr.t) (tref : t) (texpr : t) : unit =
  match (tref, texpr) with
  | NumberType, LiteralType (Val.Int _) -> ()
  | NumberType, LiteralType (Val.Flt _) -> ()
  | StringType, LiteralType (Val.Str _) -> ()
  | BooleanType, LiteralType (Val.Bool _) -> ()
  | SymbolType, LiteralType (Val.Symbol _) -> ()
  | _ ->
      let texpr_err = literal_terr expr texpr in
      T_Err.raise (T_Err.BadValue (tref, texpr_err)) ~tkn:(T_Err.Expr expr)

let check_literal_type_expr (expr : E_Expr.t) (tref : t) (texpr : t) : unit =
  match (tref, texpr) with
  | LiteralType vref, LiteralType vexpr ->
      if not (vref = vexpr) then
        T_Err.raise (T_Err.BadValue (tref, texpr)) ~tkn:(T_Err.Expr expr)
  | _, LiteralType _ -> check_literal_narrowing expr tref texpr
  | _ -> failwith "Typed ECMA-SL: T_Typing.check_literal_type"

let check_union_expr (expr : E_Expr.t) (tref : t) (texpr : t)
    (test_type_fun : t -> t -> unit) : unit =
  let check_union_expr' (ts : t list) : unit =
    try List.iter (test_type_fun tref) ts
    with T_Err.TypeError terr ->
      T_Err.push terr (T_Err.BadValue (tref, texpr))
  in
  match (tref, texpr) with
  | _, UnionType ts -> check_union_expr' ts
  | _ -> failwith "Typed ECMA-SL: T_Typing.check_union_type_expr"

let check_union_type (expr : E_Expr.t) (tref : t) (texpr : t)
    (test_type_fun : t -> t -> unit) : unit =
  let is_typeable_fun (t : t) : bool =
    try test_type_fun t texpr |> fun _ -> true with T_Err.TypeError _ -> false
  in
  let check_union_type_fun (ts : t list) : unit =
    if not (List.exists is_typeable_fun ts) then
      let texpr_err = literal_terr expr texpr in
      T_Err.raise (T_Err.BadValue (tref, texpr_err)) ~tkn:(T_Err.Expr expr)
  in
  match (tref, texpr) with
  | UnionType ts, _ -> check_union_type_fun ts
  | _ -> failwith "Typed ECMA-SL: T_Typing.check_union_type"

let check_obj_fields (expr : E_Expr.t) (otref : obj_t) (otexpr : obj_t)
    (test_type_fun : E_Expr.t -> t -> t -> unit) : unit =
  let check_expr_fld is_literal (fn, ft) =
    let terr_tkn = if is_literal then T_Err.Str fn else T_Err.Expr expr in
    let fld_opt = Hashtbl.find_opt otref.flds fn in
    match (fld_opt, is_literal) with
    | None, true -> T_Err.raise (T_Err.ExtraField fn) ~tkn:terr_tkn
    | None, false -> T_Err.raise (T_Err.ExtraField fn) ~tkn:terr_tkn
    (* FIXME : horizontal subtyping required above *)
    | Some ft', _ -> (
        let tref = E_Type.get_tfld ft' in
        match expr with
        | E_Expr.NewObj oexpr ->
            let fe = snd (List.find (fun (fn', _) -> fn' = fn) oexpr) in
            test_type_fun fe tref ft.t
        | _ -> (
            try test_type_fun expr tref ft.t
            with T_Err.TypeError terr ->
              T_Err.push terr (T_Err.IncompatibleField fn)))
  in
  let check_missing_fld flds fn ft =
    if not (Seq.exists (fun (fn', _) -> fn' = fn) flds || ft_optional ft) then
      T_Err.raise (T_Err.MissingField fn) ~tkn:(T_Err.Expr expr)
  in
  let is_literal = match expr with E_Expr.NewObj _ -> true | _ -> false in
  let flds = Hashtbl.to_seq otexpr.flds in
  Seq.iter (check_expr_fld is_literal) flds |> fun () ->
  Hashtbl.iter (check_missing_fld flds) otref.flds

let check_obj_type (expr : E_Expr.t) (tref : t) (texpr : t)
    (test_type_fun : E_Expr.t -> t -> t -> unit) : unit =
  try
    match (tref, texpr) with
    | ObjectType otref, ObjectType otexpr ->
        check_obj_fields expr otref otexpr test_type_fun
    | _ -> failwith "Typed ECMA-SL: T_Typing.check_obj_type"
  with T_Err.TypeError terr -> T_Err.push terr (T_Err.BadValue (tref, texpr))

let check_runtime_type (expr : E_Expr.t) (tref : t) (texpr : t) : unit =
  match (tref, texpr) with
  | RuntimeType Type.TypeType, RuntimeType _ -> ()
  | RuntimeType tref', RuntimeType texpr' ->
      if not (tref' = texpr') then
        T_Err.raise (T_Err.BadValue (tref, texpr)) ~tkn:(T_Err.Expr expr)
  | _ -> failwith "Typed ECMA-SL: T_Typing.check_runtime_type"

let rec test_type (expr : E_Expr.t) (tref : t) (texpr : t) : unit =
  match (tref, texpr) with
  | _, AnyType -> ()
  | AnyType, _ -> ()
  | UnknownType, _ -> ()
  | UndefinedType, UndefinedType -> ()
  | NullType, NullType -> ()
  | NumberType, NumberType -> ()
  | StringType, StringType -> ()
  | BooleanType, BooleanType -> ()
  | SymbolType, SymbolType -> ()
  | ObjectType _, ObjectType _ -> check_obj_type expr tref texpr test_type
  | _, UnionType _ -> check_union_expr expr tref texpr (test_type expr)
  | UnionType _, _ -> check_union_type expr tref texpr (test_type expr)
  | _, LiteralType _ -> check_literal_type_expr expr tref texpr
  | RuntimeType _, RuntimeType _ -> check_runtime_type expr tref texpr
  | _ -> T_Err.raise (T_Err.BadValue (tref, texpr)) ~tkn:(T_Err.Expr expr)

let is_typeable (tref : t) (texpr : t) : bool =
  try test_type (E_Expr.Val Val.Null) tref texpr |> fun _ -> true
  with T_Err.TypeError _ -> false

let type_check (expr : E_Expr.t) (tref : t) ((rtexpr, ntexpr) : t * t) : unit =
  try test_type expr tref ntexpr
  with T_Err.TypeError nterr -> test_type expr tref rtexpr

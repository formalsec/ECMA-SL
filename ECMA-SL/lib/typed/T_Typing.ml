open E_Type

let typedefs : (string, E_Type.t) Hashtbl.t ref =
  ref (Hashtbl.create !Config.default_hashtbl_sz)

let resolve_typedef (tref : E_Type.t) : E_Type.t =
  match tref with
  | UserDefinedType tname -> (
      match Hashtbl.find_opt !typedefs tname with
      | None -> T_Err.raise (T_Err.UnknownType tref) ~tkn:(T_Err.type_tkn tref)
      | Some tref' -> tref')
  | _ -> failwith "Typed ECMA-SL: T_Typing.resolve_typedef"

let literal_terr (expr : E_Expr.t) (texpr : t) : t =
  match expr with E_Expr.Val _ -> E_Type.wide_type texpr | _ -> texpr

let check_literal_narrowing (expr : E_Expr.t) (tref : t) (texpr : t) : unit =
  match (tref, texpr) with
  | IntType, LiteralType (Val.Int _) -> ()
  | FloatType, LiteralType (Val.Flt _) -> ()
  | StringType, LiteralType (Val.Str _) -> ()
  | BooleanType, LiteralType (Val.Bool _) -> ()
  | SymbolType, LiteralType (Val.Symbol _) -> ()
  | _ ->
      let texpr' = literal_terr expr texpr in
      T_Err.raise (T_Err.BadValue (tref, texpr')) ~tkn:(T_Err.expr_tkn expr)

let check_literal_expr (expr : E_Expr.t) (tref : t) (texpr : t) : unit =
  match (tref, texpr) with
  | LiteralType vref, LiteralType vexpr ->
      if not (vref = vexpr) then
        T_Err.raise (T_Err.BadValue (tref, texpr)) ~tkn:(T_Err.expr_tkn expr)
  | _, LiteralType _ -> check_literal_narrowing expr tref texpr
  | _ -> failwith "Typed ECMA-SL: T_Typing.check_literal_expr"

let check_union_expr (expr : E_Expr.t) (tref : t) (texpr : t)
    (type_check_f : t -> t -> unit) : unit =
  match (tref, texpr) with
  | _, UnionType ts -> (
      try List.iter (type_check_f tref) ts
      with T_Err.TypeError terr ->
        T_Err.push terr (T_Err.BadValue (tref, texpr)))
  | _ -> failwith "Typed ECMA-SL: T_Typing.check_union_expr"

let check_union_type (expr : E_Expr.t) (tref : t) (texpr : t)
    (type_check_f : t -> t -> unit) : unit =
  let _is_typeable_f t =
    try type_check_f t texpr |> fun _ -> true with T_Err.TypeError _ -> false
  in
  let _check_ts ts =
    if not (List.exists _is_typeable_f ts) then
      let texpr' = literal_terr expr texpr in
      T_Err.raise (T_Err.BadValue (tref, texpr')) ~tkn:(T_Err.expr_tkn expr)
  in
  match (tref, texpr) with
  | UnionType ts, _ -> _check_ts ts
  | SigmaType (_, ts), _ -> _check_ts ts
  | _ -> failwith "Typed ECMA-SL: T_Typing.check_union_type"

let check_obj_fields (expr : E_Expr.t) (toref : tobj_t) (toe : tobj_t)
    (type_check_f : E_Expr.t -> t -> t -> unit) : unit =
  let _check_expr_fld_f isLiteral (fn, (ft, _)) =
    let terrTkn = if isLiteral then T_Err.str_tkn fn else T_Err.expr_tkn expr in
    match (isLiteral, Hashtbl.find_opt toref.flds fn) with
    | true, None -> T_Err.raise (T_Err.ExtraField fn) ~tkn:terrTkn
    | false, None -> T_Err.raise (T_Err.ExtraField fn) ~tkn:terrTkn
    (* FIXME: horizontal subtyping required above *)
    | _, Some (tref, _) -> (
        match expr with
        | E_Expr.NewObj oe ->
            let fe = snd (List.find (fun (fn', _) -> fn' = fn) oe) in
            type_check_f fe tref ft
        | _ -> (
            try type_check_f expr tref ft (* FIXME: prevent field subtyping *)
            with T_Err.TypeError terr ->
              T_Err.push terr (T_Err.IncompatibleField fn)))
  in
  let _check_missing_fld_f flds fn tfld =
    if not (Seq.exists (fun (fn', _) -> fn' = fn) flds || tfld_is_opt tfld) then
      T_Err.raise (T_Err.MissingField fn) ~tkn:(T_Err.expr_tkn expr)
  in
  let isLiteral = match expr with E_Expr.NewObj _ -> true | _ -> false in
  let flds = Hashtbl.to_seq toe.flds in
  Seq.iter (_check_expr_fld_f isLiteral) flds |> fun () ->
  Hashtbl.iter (_check_missing_fld_f flds) toref.flds

let check_obj_type (expr : E_Expr.t) (tref : t) (texpr : t)
    (type_check_f : E_Expr.t -> t -> t -> unit) : unit =
  try
    match (tref, texpr) with
    | ObjectType toref, ObjectType toe ->
        check_obj_fields expr toref toe type_check_f
    | _ -> failwith "Typed ECMA-SL: T_Typing.check_tobj_type"
  with T_Err.TypeError terr -> T_Err.push terr (T_Err.BadValue (tref, texpr))

let check_runtime_type (expr : E_Expr.t) (tref : t) (texpr : t) : unit =
  match (tref, texpr) with
  | RuntimeType Type.TypeType, RuntimeType _ -> ()
  | RuntimeType tref', RuntimeType texpr' ->
      if not (tref' = texpr') then
        T_Err.raise (T_Err.BadValue (tref, texpr)) ~tkn:(T_Err.expr_tkn expr)
  | _ -> failwith "Typed ECMA-SL: T_Typing.check_runtime_type"

let check_user_type (expr : E_Expr.t) (tref : t) (texpr : t)
    (type_check_f : t -> t -> unit) : unit =
  let tref' = resolve_typedef tref in
  try type_check_f tref' texpr
  with T_Err.TypeError terr ->
    let texpr' = literal_terr expr texpr in
    T_Err.push terr (T_Err.BadValue (tref, texpr'))

let rec type_check (expr : E_Expr.t) (tref : t) (texpr : t) : unit =
  match (tref, texpr) with
  | _, AnyType -> ()
  | AnyType, _ -> ()
  | UnknownType, _ -> ()
  | NeverType, NeverType -> ()
  | UndefinedType, UndefinedType -> ()
  | NullType, NullType -> ()
  | IntType, IntType -> ()
  | FloatType, FloatType -> ()
  | StringType, StringType -> ()
  | BooleanType, BooleanType -> ()
  | SymbolType, SymbolType -> ()
  | ObjectType _, ObjectType _ -> check_obj_type expr tref texpr type_check
  | _, SigmaType (_, nts) -> type_check expr tref (UnionType nts)
  | _, UnionType _ -> check_union_expr expr tref texpr (type_check expr)
  | UnionType _, _ -> check_union_type expr tref texpr (type_check expr)
  | SigmaType _, _ -> check_union_type expr tref texpr (type_check expr)
  | RuntimeType _, RuntimeType _ -> check_runtime_type expr tref texpr
  | UserDefinedType _, UserDefinedType _ when tref = texpr -> ()
  | UserDefinedType _, _ -> check_user_type expr tref texpr (type_check expr)
  | _, LiteralType _ -> check_literal_expr expr tref texpr
  | _ -> T_Err.raise (T_Err.BadValue (tref, texpr)) ~tkn:(T_Err.expr_tkn expr)

let is_typeable (tref : t) (texpr : t) : bool =
  try type_check (E_Expr.Val Val.Null) tref texpr |> fun _ -> true
  with T_Err.TypeError _ -> false

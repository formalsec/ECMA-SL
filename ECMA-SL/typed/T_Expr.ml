open EExpr

let type_val (v : Val.t) : EType.t =
  try EType.parse_literal_type v with _ -> failwith "T_Expr.type_val"

let type_var (narrow : bool) (tctx : T_Ctx.t) (x : string) : EType.t =
  match T_Ctx.tenv_find tctx x with
  | Some tvar ->
    let (rtvar, ntvar) = T_Ctx.get_tvar_t tvar in
    if narrow then ntvar else rtvar
  | None -> T_Err.raise (T_Err.UnknownVar x) ~tkn:(T_Err.str_tkn x)

let type_const (c : Operator.const) : EType.t =
  match c with
  | Operator.MAX_VALUE -> EType.FloatType
  | Operator.MIN_VALUE -> EType.FloatType
  | Operator.PI -> EType.FloatType

let type_nopt (op : Operator.nopt) (tes : EType.t list) : EType.t =
  match op with Operator.TupleExpr -> EType.TupleType tes | _ -> EType.AnyType

let test_operand (test_operand_f : EExpr.t -> EType.t -> EType.t)
  ((arg, tparam) : EExpr.t * EType.t) : unit =
  try ignore (test_operand_f arg tparam)
  with T_Err.TypeError terr -> (
    match T_Err.top_err terr with
    | T_Err.BadValue (tref, texpr) ->
      T_Err.update terr (T_Err.BadOperand (tref, texpr))
    | _ -> T_Err.continue terr )

let test_arg (test_arg_f : EExpr.t -> EType.t -> EType.t)
  ((arg, tparam) : EExpr.t * EType.t) : unit =
  try ignore (test_arg_f arg tparam)
  with T_Err.TypeError terr -> (
    match T_Err.top_err terr with
    | T_Err.BadValue (tref, texpr) ->
      T_Err.update terr (T_Err.BadArgument (tref, texpr))
    | _ -> T_Err.continue terr )

let test_generic_call (_tctx : T_Ctx.t) (args : EExpr.t list)
  (tparams : EType.t list) (tret : EType.t) (test_f : EExpr.t * EType.t -> unit)
  : EType.t =
  List.combine args tparams |> List.iter test_f |> fun () ->
  T_Narrowing.narrow_type tret

let test_operator_call (tctx : T_Ctx.t) (args : EExpr.t list)
  (fProtos : T_Op.funcPrototype_t list) (test_f : EExpr.t * EType.t -> unit) :
  EType.t =
  let _test_call_f (tparams, tret) (rSucc, rErr) =
    try (test_generic_call tctx args tparams tret test_f :: rSucc, rErr)
    with T_Err.TypeError terr -> (rSucc, terr :: rErr)
  in
  let (succCalls, errCalls) = List.fold_right _test_call_f fProtos ([], []) in
  match (succCalls, errCalls) with
  | (t :: [], _) -> t
  | (_t :: _, _) -> EType.AnyType
  | ([], terr :: _) -> T_Err.continue terr
  | _ -> failwith "Typed ECMA-SL: T_Expr.test_operator_call"

let test_nargs (_tctx : T_Ctx.t) (expr : EExpr.t) (args : EExpr.t list)
  (tparams : EType.t list) : unit =
  let (nparams, nargs) = (List.length tparams, List.length args) in
  if nparams != nargs then
    T_Err.raise
      (T_Err.NExpectedArgs (nparams, nargs))
      ~tkn:(T_Err.expr_tkn expr)

let type_named_call (tctx : T_Ctx.t) (expr : EExpr.t) (fname : string)
  (args : EExpr.t list) : EType.t list * EType.t =
  let _check_abrupt tret =
    if tret = EType.NeverType then T_Ctx.set_tstate tctx T_Ctx.Abrupt
  in
  match T_Ctx.get_func_by_name tctx fname with
  | Some func ->
    let tparams = EFunc.get_tparams func in
    let tret = Option.value ~default:EType.AnyType (EFunc.get_return_t func) in
    let _ = _check_abrupt tret in
    test_nargs tctx expr args tparams |> fun () -> (tparams, tret)
  | None -> T_Err.raise (T_Err.UnknownFunction fname) ~tkn:(T_Err.str_tkn fname)

let type_call (tctx : T_Ctx.t) (expr : EExpr.t) (fexpr : EExpr.t)
  (args : EExpr.t list) : EType.t list * EType.t =
  match fexpr with
  | EExpr.Val (Val.Str fname) -> type_named_call tctx expr fname args
  | _ -> (List.map (fun _ -> EType.AnyType) args, EType.AnyType)

let type_newobj (_tctx : T_Ctx.t) (tfes : (string * EType.t) list) : EType.t =
  let _type_obj_field_f flds (fn, ft) =
    match Hashtbl.find_opt flds fn with
    | None -> Hashtbl.add flds fn (ft, EType.Required)
    | Some _ -> T_Err.raise (T_Err.DuplicatedField fn) ~tkn:(T_Err.str_tkn fn)
  in
  let flds = Hashtbl.create !Config.default_hashtbl_sz in
  List.iter (_type_obj_field_f flds) tfes |> fun () ->
  EType.ObjectType { EType.flds; EType.smry = None }

let rec type_fld_lookup (oe : EExpr.t) (fe : EExpr.t) (fn : string) (t : EType.t)
  : EType.t =
  let _terr_obj msg = T_Err.raise msg ~tkn:(T_Err.expr_tkn oe) in
  let _terr_fexpr msg = T_Err.raise msg ~tkn:(T_Err.expr_tkn fe) in
  let _objName = failwith "TODO: Get object name" in
  match t with
  | EType.AnyType -> EType.AnyType
  | EType.UnknownType -> _terr_obj (T_Err.BadType (_objName, t))
  | EType.UndefinedType -> _terr_obj (T_Err.BadPossibleType (_objName, t))
  | EType.NullType -> _terr_obj (T_Err.BadPossibleType (_objName, t))
  | EType.ObjectType tobj -> (
    match EType.find_tfld_opt tobj fn with
    | Some tfld -> EType.tfld_t tfld
    | None -> _terr_fexpr (T_Err.BadLookup (fn, t)) )
  | EType.UserDefinedType _ ->
    let t' = T_Typing.resolve_typedef t in
    type_fld_lookup oe fe fn t'
  | _ -> _terr_fexpr (T_Err.BadLookup (fn, t))

let rec type_lookup (narrow : bool) (tctx : T_Ctx.t) (oe : EExpr.t)
  (fe : EExpr.t) (toe : EType.t) : EType.t =
  let _type_fld_lookup unionLookup fn tobj =
    try type_fld_lookup oe fe fn tobj
    with T_Err.TypeError terr -> (
      match (unionLookup, T_Err.top_err terr) with
      | (true, T_Err.BadLookup (fn, _t)) ->
        T_Err.push terr (T_Err.BadLookup (fn, toe))
      | _ -> T_Err.continue terr )
  in
  let _type_union_lookup fn ts =
    List.map (_type_fld_lookup true fn) ts
    |> EType.merge_type EType.merge_union_type
    |> fun t -> if narrow then T_Narrowing.narrow_type t else t
  in
  match (fe, toe) with
  | (EExpr.Val (Val.Str fn), EType.UnionType ts) -> _type_union_lookup fn ts
  | (EExpr.Val (Val.Str fn), EType.SigmaType (d, ts)) ->
    _type_union_lookup fn ts |> EType.union_to_sigma d
  | (EExpr.Val (Val.Str _fn), EType.UserDefinedType _) ->
    type_lookup narrow tctx oe fe (T_Typing.resolve_typedef toe)
  | (EExpr.Val (Val.Str fn), _) -> _type_fld_lookup false fn toe
  | _ -> EType.AnyType

let rec type_expr ?(narrow : bool = true) (tctx : T_Ctx.t) (expr : EExpr.t) :
  EType.t =
  match expr with
  | Val v -> type_val v
  | Var x -> type_var narrow tctx x
  (* | GVar _ ->  *)
  | Const c -> type_const c
  | UnOpt (op, e) ->
    let args = [ e ] in
    let funPrototype = T_Op.type_unop op in
    let test_operand_f = test_operand (safe_type_expr tctx) in
    test_operator_call tctx args funPrototype test_operand_f
  | BinOpt (op, e1, e2) ->
    let args = [ e1; e2 ] in
    let funPrototype = T_Op.type_binop op in
    let test_operand_f = test_operand (safe_type_expr tctx) in
    test_operator_call tctx args funPrototype test_operand_f
  | TriOpt (op, e1, e2, e3) ->
    let args = [ e1; e2; e3 ] in
    let funPrototype = T_Op.type_triop op in
    let test_operand_f = test_operand (safe_type_expr tctx) in
    test_operator_call tctx args funPrototype test_operand_f
  | NOpt (op, es) ->
    let tes = List.map (type_expr ~narrow tctx) es in
    type_nopt op tes
  | Call (fexpr, args, _) ->
    let (tparams, tret) = type_call tctx expr fexpr args in
    let test_arg_f = test_arg (safe_type_expr tctx) in
    test_generic_call tctx args tparams tret test_arg_f
  (* | ECall (_, _) ->  *)
  | NewObj fes ->
    let tfes = List.map (fun (fn, e) -> (fn, type_expr tctx e)) fes in
    type_newobj tctx tfes
  | Lookup (oe, fe) ->
    let toe = type_expr ~narrow tctx oe in
    type_lookup narrow tctx oe fe toe
  (* | Curry (_, _) ->  *)
  (* | Symbolic (_, _) ->  *)
  | _ -> EType.AnyType

and safe_type_expr (tctx : T_Ctx.t) (expr : EExpr.t) (tref : EType.t) : EType.t
    =
  try
    let ntexpr = type_expr tctx expr in
    T_Typing.type_check expr tref ntexpr |> fun () -> ntexpr
  with T_Err.TypeError _ -> (
    try
      let rtexpr = type_expr ~narrow:false tctx expr in
      T_Typing.type_check expr tref rtexpr |> fun () ->
      failwith "Typed ECMA-SL: T_Expr.safe_type_expr"
    with T_Err.TypeError terr -> T_Err.continue terr )

let full_type_expr (tctx : T_Ctx.t) (expr : EExpr.t) : EType.t * EType.t =
  let rtexpr = type_expr ~narrow:false tctx expr in
  let ntexpr = type_expr tctx expr in
  (rtexpr, ntexpr)

let full_type_expr_resolved (tctx : T_Ctx.t) (expr : EExpr.t) :
  EType.t * EType.t =
  let _resolve_typedef t =
    match t with
    | EType.UserDefinedType _ -> T_Typing.resolve_typedef t
    | _ -> t
  in
  full_type_expr tctx expr |> fun (rt, nt) ->
  (_resolve_typedef rt, _resolve_typedef nt)

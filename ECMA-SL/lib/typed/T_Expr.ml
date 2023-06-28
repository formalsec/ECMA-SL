open E_Expr

let type_val (v : Val.t) : E_Type.t =
  try E_Type.parse_literal_type v with _ -> failwith "T_Expr.type_val"

let type_var (narrow : bool) (tctx : T_Ctx.t) (x : string) : E_Type.t =
  match T_Ctx.tenv_find tctx x with
  | Some tvar ->
      let rtvar, ntvar = T_Ctx.get_tvar_t tvar in
      if narrow then ntvar else rtvar
  | None -> T_Err.raise (T_Err.UnknownVar x) ~tkn:(T_Err.str_tkn x)

let type_const (c : Operators.const) : E_Type.t =
  match c with
  | Operators.MAX_VALUE -> E_Type.FloatType
  | Operators.MIN_VALUE -> E_Type.FloatType
  | Operators.PI -> E_Type.FloatType

let test_operand (test_operand_f : E_Expr.t -> E_Type.t -> E_Type.t)
    ((arg, tparam) : E_Expr.t * E_Type.t) : unit =
  try ignore (test_operand_f arg tparam)
  with T_Err.TypeError terr -> (
    match T_Err.top_err terr with
    | T_Err.BadValue (tref, texpr) ->
        T_Err.update terr (T_Err.BadOperand (tref, texpr))
    | _ -> T_Err.continue terr)

let test_arg (test_arg_f : E_Expr.t -> E_Type.t -> E_Type.t)
    ((arg, tparam) : E_Expr.t * E_Type.t) : unit =
  try ignore (test_arg_f arg tparam)
  with T_Err.TypeError terr -> (
    match T_Err.top_err terr with
    | T_Err.BadValue (tref, texpr) ->
        T_Err.update terr (T_Err.BadArgument (tref, texpr))
    | _ -> T_Err.continue terr)

let test_generic_call (tctx : T_Ctx.t) (args : E_Expr.t list)
    (tparams : E_Type.t list) (tret : E_Type.t)
    (test_f : E_Expr.t * E_Type.t -> unit) : E_Type.t =
  List.combine args tparams |> List.iter test_f |> fun () ->
  T_Narrowing.narrow_type tret

let test_operator_call (tctx : T_Ctx.t) (args : E_Expr.t list)
    (fProtos : T_Op.funcPrototype_t list) (test_f : E_Expr.t * E_Type.t -> unit)
    : E_Type.t =
  let _test_call_f (tparams, tret) (rSucc, rErr) =
    try (test_generic_call tctx args tparams tret test_f :: rSucc, rErr)
    with T_Err.TypeError terr -> (rSucc, terr :: rErr)
  in
  let succCalls, errCalls = List.fold_right _test_call_f fProtos ([], []) in
  match (succCalls, errCalls) with
  | t :: [], _ -> t
  | t :: _, _ -> E_Type.AnyType
  | [], terr :: _ -> T_Err.continue terr
  | _ -> failwith "Typed ECMA-SL: T_Expr.test_operator_call"

let test_nargs (tctx : T_Ctx.t) (expr : E_Expr.t) (args : E_Expr.t list)
    (tparams : E_Type.t list) : unit =
  let nparams, nargs = (List.length tparams, List.length args) in
  if nparams != nargs then
    T_Err.raise
      (T_Err.NExpectedArgs (nparams, nargs))
      ~tkn:(T_Err.expr_tkn expr)

let type_named_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fname : string)
    (args : E_Expr.t list) : E_Type.t list * E_Type.t =
  match T_Ctx.get_func_by_name tctx fname with
  | Some func ->
      let tparams = E_Func.get_tparams func in
      let tret = Option.default E_Type.AnyType (E_Func.get_return_t func) in
      test_nargs tctx expr args tparams |> fun () -> (tparams, tret)
  | None -> T_Err.raise (T_Err.UnknownFunction fname) ~tkn:(T_Err.str_tkn fname)

let type_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fexpr : E_Expr.t)
    (args : E_Expr.t list) : E_Type.t list * E_Type.t =
  match fexpr with
  | E_Expr.Val (Val.Str fname) -> type_named_call tctx expr fname args
  | _ -> (List.map (fun _ -> E_Type.AnyType) args, E_Type.AnyType)

let type_newobj (tctx : T_Ctx.t) (tfes : (string * E_Type.t) list) : E_Type.t =
  let _type_obj_field_f flds (fn, ft) =
    match Hashtbl.find_opt flds fn with
    | None -> Hashtbl.add flds fn (ft, E_Type.Required)
    | Some _ -> T_Err.raise (T_Err.DuplicatedField fn) ~tkn:(T_Err.str_tkn fn)
  in
  let flds = Hashtbl.create !Config.default_hashtbl_sz in
  List.iter (_type_obj_field_f flds) tfes |> fun () ->
  E_Type.ObjectType { E_Type.flds; E_Type.smry = None }

let type_fld_lookup (oe : E_Expr.t) (fe : E_Expr.t) (fn : string) (t : E_Type.t)
    : E_Type.t =
  let _terr_obj msg = T_Err.raise msg ~tkn:(T_Err.expr_tkn oe) in
  let _terr_fexpr msg = T_Err.raise msg ~tkn:(T_Err.expr_tkn fe) in
  let objName = E_Expr.get_expr_name oe in
  match t with
  | E_Type.AnyType -> E_Type.AnyType
  | E_Type.UnknownType -> _terr_obj (T_Err.BadType (objName, t))
  | E_Type.UndefinedType -> _terr_obj (T_Err.BadPossibleType (objName, t))
  | E_Type.NullType -> _terr_obj (T_Err.BadPossibleType (objName, t))
  | E_Type.ObjectType tobj -> (
      match E_Type.find_tfld_opt tobj fn with
      | Some tfld -> E_Type.tfld_t tfld
      | None -> _terr_fexpr (T_Err.BadLookup (fn, t)))
  | _ -> _terr_fexpr (T_Err.BadLookup (fn, t))

let type_lookup (narrow : bool) (tctx : T_Ctx.t) (oe : E_Expr.t) (fe : E_Expr.t)
    (toe : E_Type.t) : E_Type.t =
  let _union_to_sigma d t =
    match t with E_Type.UnionType ts -> E_Type.SigmaType (d, ts) | _ -> t
  in
  let _type_fld_lookup fn tobj =
    try type_fld_lookup oe fe fn tobj
    with T_Err.TypeError terr -> (
      match T_Err.top_err terr with
      | T_Err.BadLookup (fn, t) -> T_Err.push terr (T_Err.BadLookup (fn, toe))
      | _ -> T_Err.continue terr)
  in
  let _type_union_lookup fn ts =
    List.map (_type_fld_lookup fn) ts
    |> E_Type.merge_type E_Type.merge_union_type
    |> fun t -> if narrow then T_Narrowing.narrow_type t else t
  in
  match (fe, toe) with
  | E_Expr.Val (Val.Str fn), E_Type.SigmaType (d, ts) ->
      _type_union_lookup fn ts |> _union_to_sigma d
  | E_Expr.Val (Val.Str fn), E_Type.UnionType ts -> _type_union_lookup fn ts
  | E_Expr.Val (Val.Str fn), _ -> _type_fld_lookup fn toe
  | _ -> E_Type.AnyType

let rec type_expr ?(narrow : bool = true) (tctx : T_Ctx.t) (expr : E_Expr.t) :
    E_Type.t =
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
  | EBinOpt (op, e1, e2) ->
      let args = [ e1; e2 ] in
      let funPrototype = T_Op.type_ebinop op in
      let test_operand_f = test_operand (safe_type_expr tctx) in
      test_operator_call tctx args funPrototype test_operand_f
  | TriOpt (op, e1, e2, e3) ->
      let args = [ e1; e2; e3 ] in
      let funPrototype = T_Op.type_triop op in
      let test_operand_f = test_operand (safe_type_expr tctx) in
      test_operator_call tctx args funPrototype test_operand_f
  (* | NOpt (_, _) ->  *)
  | Call (fexpr, args, _) ->
      let tparams, tret = type_call tctx expr fexpr args in
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
  | _ -> E_Type.AnyType

and safe_type_expr (tctx : T_Ctx.t) (expr : E_Expr.t) (tref : E_Type.t) :
    E_Type.t =
  try
    let ntexpr = type_expr tctx expr in
    T_Typing.type_check expr tref ntexpr |> fun () -> ntexpr
  with T_Err.TypeError _ -> (
    try
      let rtexpr = type_expr ~narrow:false tctx expr in
      T_Typing.type_check expr tref rtexpr |> fun () ->
      failwith "Typed ECMA-SL: T_Expr.safe_type_expr"
    with T_Err.TypeError terr -> T_Err.continue terr)

let full_type_expr (tctx : T_Ctx.t) (expr : E_Expr.t) : E_Type.t * E_Type.t =
  let rtexpr = type_expr ~narrow:false tctx expr in
  let ntexpr = type_expr tctx expr in
  (rtexpr, ntexpr)

open E_Expr

type texpr_t = E_Type.t * E_Type.t

let type_val (expr : Val.t) : E_Type.t =
  match expr with
  | Val.Flt _ -> E_Type.LiteralType expr
  | Val.Int _ -> E_Type.LiteralType expr
  | Val.Str _ -> E_Type.LiteralType expr
  | Val.Bool _ -> E_Type.LiteralType expr
  | Val.Symbol "undefined" -> E_Type.UndefinedType
  | Val.Symbol _ -> E_Type.LiteralType expr
  | Val.Null -> E_Type.NullType
  | _ -> E_Type.AnyType

let type_var (tctx : T_Ctx.t) (var : string) : texpr_t =
  match T_Ctx.tenv_find tctx var with
  | Some tvar -> (T_Ctx.tvar_tref tvar, T_Ctx.tvar_tnarrow tvar)
  | None -> T_Err.raise (T_Err.UnknownVar var) ~tkn:(T_Err.Str var)

let type_const (const : Operators.const) : E_Type.t =
  match const with
  | Operators.MAX_VALUE -> E_Type.NumberType
  | Operators.MIN_VALUE -> E_Type.NumberType
  | Operators.PI -> E_Type.NumberType

let type_operand (arg : E_Expr.t) (tparam : E_Type.t) (targ : texpr_t) : unit =
  try ignore (T_Typing.type_check arg tparam targ)
  with T_Err.TypeError terr -> (
    match terr.T_Err.errs with
    | T_Err.BadValue (tref, texpr) :: _ ->
        T_Err.update terr (T_Err.BadOperand (tref, texpr))
    | _ -> failwith "Typed ECMA-SL: T_Expr.type_operand")

let type_operator (args : E_Expr.t list) (tparams : E_Type.t list)
    (tret : E_Type.t) (type_expr_fun : E_Expr.t -> texpr_t) : texpr_t =
  let targs = List.map (fun arg -> type_expr_fun arg) args in
  let param_args = List.combine args (List.combine tparams targs) in
  let type_operand_fun (arg, (tparam, targ)) = type_operand arg tparam targ in
  let _ = List.iter type_operand_fun param_args in
  (tret, T_Narrowing.type_narrowing tret)

let type_arg (arg : E_Expr.t) (tparam : E_Type.t) (targ : texpr_t) : unit =
  try ignore (T_Typing.type_check arg tparam targ)
  with T_Err.TypeError terr -> (
    match terr.T_Err.errs with
    | T_Err.BadValue (tref, texpr) :: _ ->
        T_Err.update terr (T_Err.BadArgument (tref, texpr))
    | _ -> failwith "Typed ECMA-SL: T_Expr.type_arg")

let type_args (tctx : T_Ctx.t) (expr : E_Expr.t) (args : E_Expr.t list)
    (tparams : E_Type.t list) (targs : texpr_t list) : unit =
  let nparams = List.length tparams in
  let nargs = List.length targs in
  if nparams != nargs then
    T_Err.raise (T_Err.NExpectedArgs (nparams, nargs)) ~tkn:(T_Err.Expr expr)
  else
    let param_args = List.combine args (List.combine tparams targs) in
    List.iter (fun (arg, (tparam, targ)) -> type_arg arg tparam targ) param_args

let type_named_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fname : string)
    (args : E_Expr.t list) (targs : texpr_t list) : E_Type.t =
  match T_Ctx.get_func_by_name tctx fname with
  | None -> T_Err.raise (T_Err.UnknownFunction fname) ~tkn:(T_Err.Str fname)
  | Some func' ->
      let tparams = E_Func.get_tparams func' in
      let _ = type_args tctx expr args tparams targs in
      Option.default E_Type.AnyType (E_Func.get_return_t func')

let type_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fexpr : E_Expr.t)
    (args : E_Expr.t list) (targs : texpr_t list) : E_Type.t =
  match fexpr with
  | E_Expr.Val (Val.Str fname) -> type_named_call tctx expr fname args targs
  | _ -> E_Type.AnyType

let type_newobj (tctx : T_Ctx.t) (tfes : (string * texpr_t) list) : E_Type.t =
  let type_obj_field (flds : (string, E_Type.field_t) Hashtbl.t)
      ((fn, (_, nft)) : string * texpr_t) =
    let status = E_Type.Required in
    match Hashtbl.find_opt flds fn with
    | None -> Hashtbl.add flds fn { E_Type.t = nft; status }
    | Some _ -> T_Err.raise (T_Err.DuplicatedField fn) ~tkn:(T_Err.Str fn)
  in
  let flds = Hashtbl.create !Config.default_hashtbl_sz in
  List.iter (type_obj_field flds) tfes |> fun () ->
  E_Type.ObjectType { E_Type.flds; E_Type.smry = None }

let type_fld_lookup (oexpr : E_Expr.t) (fexpr : E_Expr.t) (fn : string)
    (tobj : E_Type.t) : E_Type.t =
  let obj_name () = E_Expr.get_expr_name oexpr in
  let terr_obj terr_msg = T_Err.raise terr_msg ~tkn:(T_Err.Expr oexpr) in
  let terr_fexpr terr_msg = T_Err.raise terr_msg ~tkn:(T_Err.Expr fexpr) in
  match tobj with
  | E_Type.AnyType -> E_Type.AnyType
  | E_Type.UnknownType -> terr_obj (T_Err.BadType (obj_name (), tobj))
  | E_Type.UndefinedType -> terr_obj (T_Err.BadPossibleType (obj_name (), tobj))
  | E_Type.NullType -> terr_obj (T_Err.BadPossibleType (obj_name (), tobj))
  | E_Type.ObjectType tobj' -> (
      match Hashtbl.find_opt tobj'.E_Type.flds fn with
      | None -> terr_fexpr (T_Err.BadLookup (fn, tobj))
      | Some ft -> E_Type.get_tfld ft)
  | _ -> terr_fexpr (T_Err.BadLookup (fn, tobj))

let type_lookup (tctx : T_Ctx.t) (oexpr : E_Expr.t) ((rtobj, ntobj) : texpr_t)
    (fexpr : E_Expr.t) : texpr_t =
  let type_fld_lookup_fun raise fexpr fn t =
    try type_fld_lookup oexpr fexpr fn t
    with T_Err.TypeError terr -> (
      match (raise, terr.T_Err.errs) with
      | true, T_Err.BadLookup (fn, t) :: _ ->
          T_Err.raise (T_Err.BadLookup (fn, rtobj))
      | true, _ -> T_Err.continue terr
      | false, _ -> E_Type.UndefinedType)
  in
  let type_union_lookup ?(narrow = false) raise ts fn =
    let tlookup = List.map (type_fld_lookup_fun raise fexpr fn) ts in
    let texpr = E_Type.merge_type E_Type.merge_union_type tlookup in
    if narrow then T_Narrowing.type_narrowing texpr else texpr
  in
  match (fexpr, rtobj, ntobj) with
  | E_Expr.Val (Val.Str fn), E_Type.UnionType rts, E_Type.UnionType nts ->
      let rtexpr = type_union_lookup false rts fn in
      let ntexpr = type_union_lookup ~narrow:true true nts fn in
      (rtexpr, ntexpr)
  | E_Expr.Val (Val.Str fn), E_Type.UnionType rts, _ ->
      let rtexpr = type_union_lookup false rts fn in
      let ntexpr = type_fld_lookup_fun true fexpr fn ntobj in
      (rtexpr, ntexpr)
  | E_Expr.Val (Val.Str fn), _, _ ->
      let rtexpr = type_fld_lookup_fun false fexpr fn rtobj in
      let ntexpr = type_fld_lookup_fun true fexpr fn ntobj in
      (rtexpr, ntexpr)
  | _ -> (E_Type.AnyType, E_Type.AnyType)

let rec type_expr (tctx : T_Ctx.t) (expr : E_Expr.t) : texpr_t =
  match expr with
  | Val v -> type_val v |> fun texpr -> (texpr, texpr)
  | Var x -> type_var tctx x
  (* | GVar _ ->  *)
  | Const c -> type_const c |> fun texpr -> (texpr, texpr)
  | UnOpt (op, e) ->
      let args = [ e ] in
      let tparams, tret = T_Op.type_unop op in
      type_operator args tparams tret (type_expr tctx)
  | BinOpt (op, e1, e2) ->
      let args = [ e1; e2 ] in
      let tparams, tret = T_Op.type_binop op in
      type_operator args tparams tret (type_expr tctx)
  | EBinOpt (op, e1, e2) ->
      let args = [ e1; e2 ] in
      let tparams, tret = T_Op.type_ebinop op in
      type_operator args tparams tret (type_expr tctx)
  | TriOpt (op, e1, e2, e3) ->
      let args = [ e1; e2; e3 ] in
      let tparams, tret = T_Op.type_triop op in
      type_operator args tparams tret (type_expr tctx)
  (* | NOpt (_, _) ->  *)
  | Call (fexpr, args, _) ->
      let targs = List.map (fun arg -> type_expr tctx arg) args in
      let tret = type_call tctx expr fexpr args targs in
      (tret, T_Narrowing.type_narrowing tret)
  (* | ECall (_, _) ->  *)
  | NewObj fes ->
      let tfes = List.map (fun (fn, ft) -> (fn, type_expr tctx ft)) fes in
      type_newobj tctx tfes |> fun tobj -> (tobj, tobj)
  | Lookup (oe, fe) ->
      let tobj = type_expr tctx oe in
      type_lookup tctx oe tobj fe
  (* | Curry (_, _) ->  *)
  (* | Symbolic (_, _) ->  *)
  | _ -> (E_Type.AnyType, E_Type.AnyType)

open E_Expr

type texpr_t = E_Type.t * E_Type.t

let type_val (v : Val.t) : E_Type.t =
  match v with
  | Val.Null -> E_Type.NullType
  | Val.Int _ -> E_Type.LiteralType v
  | Val.Flt _ -> E_Type.LiteralType v
  | Val.Str _ -> E_Type.LiteralType v
  | Val.Bool _ -> E_Type.LiteralType v
  | Val.Symbol "undefined" -> E_Type.UndefinedType
  | Val.Symbol _ -> E_Type.LiteralType v
  | Val.Type t -> E_Type.RuntimeType t
  | _ -> E_Type.AnyType

let type_var (tctx : T_Ctx.t) (x : string) : texpr_t =
  match T_Ctx.tenv_find tctx x with
  | Some tvar -> T_Ctx.get_tvar_t tvar
  | None -> T_Err.raise (T_Err.UnknownVar x) ~tkn:(T_Err.Str x)

let type_const (c : Operators.const) : E_Type.t =
  match c with
  | Operators.MAX_VALUE -> E_Type.NumberType
  | Operators.MIN_VALUE -> E_Type.NumberType
  | Operators.PI -> E_Type.NumberType

let type_operand ((arg, (tparam, targ)) : E_Expr.t * (E_Type.t * texpr_t)) :
    unit =
  try ignore (T_Typing.type_check arg tparam targ)
  with T_Err.TypeError terr -> (
    match terr.T_Err.errs with
    | T_Err.BadValue (tref, texpr) :: _ ->
        T_Err.update terr (T_Err.BadOperand (tref, texpr))
    | _ -> failwith "Typed ECMA-SL: T_Expr.type_operand")

let type_operator (tparams : E_Type.t list) (tret : E_Type.t)
    (targs : texpr_t list) (args : E_Expr.t list) : texpr_t =
  List.iter type_operand (List.combine args (List.combine tparams targs))
  |> fun () -> (tret, T_Narrowing.narrow_type tret)

let type_arg ((arg, (tparam, targ)) : E_Expr.t * (E_Type.t * texpr_t)) : unit =
  try ignore (T_Typing.type_check arg tparam targ)
  with T_Err.TypeError terr -> (
    match terr.T_Err.errs with
    | T_Err.BadValue (tref, texpr) :: _ ->
        T_Err.update terr (T_Err.BadArgument (tref, texpr))
    | _ -> failwith "Typed ECMA-SL: T_Expr.type_arg")

let type_args (tctx : T_Ctx.t) (expr : E_Expr.t) (args : E_Expr.t list)
    (tparams : E_Type.t list) (targs : texpr_t list) : unit =
  let nparams, nargs = (List.length tparams, List.length targs) in
  if nparams == nargs then
    List.iter type_arg (List.combine args (List.combine tparams targs))
  else T_Err.raise (T_Err.NExpectedArgs (nparams, nargs)) ~tkn:(T_Err.Expr expr)

let type_named_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fname : string)
    (args : E_Expr.t list) (targs : texpr_t list) : E_Type.t =
  match T_Ctx.get_func_by_name tctx fname with
  | Some func ->
      let tparams = E_Func.get_tparams func in
      let _ = type_args tctx expr args tparams targs in
      Option.default E_Type.AnyType (E_Func.get_return_t func)
  | None -> T_Err.raise (T_Err.UnknownFunction fname) ~tkn:(T_Err.Str fname)

let type_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fexpr : E_Expr.t)
    (args : E_Expr.t list) (targs : texpr_t list) : E_Type.t =
  match fexpr with
  | E_Expr.Val (Val.Str fname) -> type_named_call tctx expr fname args targs
  | _ -> E_Type.AnyType

let type_newobj (tctx : T_Ctx.t) (tfes : (string * texpr_t) list) : E_Type.t =
  let _type_obj_field_f flds (fn, (_, ft)) =
    match Hashtbl.find_opt flds fn with
    | None -> Hashtbl.add flds fn (ft, E_Type.Required)
    | Some _ -> T_Err.raise (T_Err.DuplicatedField fn) ~tkn:(T_Err.Str fn)
  in
  let flds = Hashtbl.create !Config.default_hashtbl_sz in
  List.iter (_type_obj_field_f flds) tfes |> fun () ->
  E_Type.ObjectType { E_Type.flds; E_Type.smry = None }

let type_fld_lookup (oe : E_Expr.t) (fe : E_Expr.t) (fn : string)
    (tobj : E_Type.t) : E_Type.t =
  let _terr_obj msg = T_Err.raise msg ~tkn:(T_Err.Expr oe) in
  let _terr_fexpr msg = T_Err.raise msg ~tkn:(T_Err.Expr fe) in
  let objName = E_Expr.get_expr_name oe in
  match tobj with
  | E_Type.AnyType -> E_Type.AnyType
  | E_Type.UnknownType -> _terr_obj (T_Err.BadType (objName, tobj))
  | E_Type.UndefinedType -> _terr_obj (T_Err.BadPossibleType (objName, tobj))
  | E_Type.NullType -> _terr_obj (T_Err.BadPossibleType (objName, tobj))
  | E_Type.ObjectType tobj' -> (
      match E_Type.get_tfld tobj' fn with
      | Some ft -> E_Type.get_fld_t ft
      | None -> _terr_fexpr (T_Err.BadLookup (fn, tobj)))
  | _ -> _terr_fexpr (T_Err.BadLookup (fn, tobj))

let type_lookup (tctx : T_Ctx.t) (oe : E_Expr.t) (fe : E_Expr.t)
    ((rtoexpr, ntoexpr) : texpr_t) : texpr_t =
  let _type_fld_lookup raiseErr fn tobj =
    try type_fld_lookup oe fe fn tobj
    with T_Err.TypeError terr -> (
      match (raiseErr, terr.T_Err.errs) with
      | true, T_Err.BadLookup (fn, t) :: _ ->
          T_Err.raise (T_Err.BadLookup (fn, rtoexpr))
      | true, _ -> T_Err.continue terr
      | false, _ -> E_Type.UndefinedType)
  in
  let _type_union_lookup narrowResult ts fn =
    List.map (_type_fld_lookup narrowResult fn) ts
    |> E_Type.merge_type E_Type.merge_union_type
    |> fun t -> if narrowResult then T_Narrowing.narrow_type t else t
  in
  match (fe, rtoexpr, ntoexpr) with
  | E_Expr.Val (Val.Str fn), E_Type.UnionType rts, E_Type.UnionType nts ->
      let rt = _type_union_lookup false rts fn in
      let nt = _type_union_lookup true nts fn in
      (rt, nt)
  | E_Expr.Val (Val.Str fn), E_Type.UnionType rts, _ ->
      let rt = _type_union_lookup false rts fn in
      let nt = _type_fld_lookup true fn ntoexpr in
      (rt, nt)
  | E_Expr.Val (Val.Str fn), _, _ ->
      let rt = _type_fld_lookup false fn rtoexpr in
      let nt = _type_fld_lookup true fn ntoexpr in
      (rt, nt)
  | _ -> (E_Type.AnyType, E_Type.AnyType)

let rec type_expr (tctx : T_Ctx.t) (expr : E_Expr.t) : texpr_t =
  match expr with
  | Val v -> type_val v |> fun texpr -> (texpr, texpr)
  | Var x -> type_var tctx x
  (* | GVar _ ->  *)
  | Const c -> type_const c |> fun texpr -> (texpr, texpr)
  | UnOpt (op, e) ->
      let args = [ e ] in
      let targs = List.map (type_expr tctx) args in
      let tparams, tret = T_Op.type_unop op in
      type_operator tparams tret targs args
  | BinOpt (op, e1, e2) ->
      let args = [ e1; e2 ] in
      let targs = List.map (type_expr tctx) args in
      let tparams, tret = T_Op.type_binop op in
      type_operator tparams tret targs args
  | EBinOpt (op, e1, e2) ->
      let args = [ e1; e2 ] in
      let targs = List.map (type_expr tctx) args in
      let tparams, tret = T_Op.type_ebinop op in
      type_operator tparams tret targs args
  | TriOpt (op, e1, e2, e3) ->
      let args = [ e1; e2; e3 ] in
      let targs = List.map (type_expr tctx) args in
      let tparams, tret = T_Op.type_triop op in
      type_operator tparams tret targs args
  (* | NOpt (_, _) ->  *)
  | Call (fexpr, args, _) ->
      let targs = List.map (type_expr tctx) args in
      let tret = type_call tctx expr fexpr args targs in
      (tret, T_Narrowing.narrow_type tret)
  (* | ECall (_, _) ->  *)
  | NewObj fes ->
      let tfes = List.map (fun (fn, ft) -> (fn, type_expr tctx ft)) fes in
      type_newobj tctx tfes |> fun tobj -> (tobj, tobj)
  | Lookup (oe, fe) ->
      let toexpr = type_expr tctx oe in
      type_lookup tctx oe fe toexpr
  (* | Curry (_, _) ->  *)
  (* | Symbolic (_, _) ->  *)
  | _ -> (E_Type.AnyType, E_Type.AnyType)

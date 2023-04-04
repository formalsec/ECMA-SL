open E_Expr

let type_val (expr : Val.t) : E_Type.t =
  match expr with
  | Val.Flt _ -> E_Type.NumberType
  | Val.Int _ -> E_Type.NumberType
  | Val.Str _ -> E_Type.StringType
  | Val.Bool _ -> E_Type.BooleanType
  | Val.Symbol "undefined" -> E_Type.UndefinedType
  | Val.Symbol _ -> E_Type.SymbolType
  | default -> E_Type.AnyType

let type_var (tctx : T_Ctx.t) (var : string) : E_Type.t =
  match T_Ctx.tenv_find tctx var with
  | Some tvar -> tvar
  | None -> T_Err.raise (T_Err.UnknownVar var) ~cs:(T_Err.Str var)

let type_const (const : Operators.const) : E_Type.t =
  match const with
  | Operators.MAX_VALUE -> E_Type.NumberType
  | Operators.MIN_VALUE -> E_Type.NumberType
  | Operators.PI -> E_Type.NumberType

let type_unop (tctx : T_Ctx.t) (expr : E_Expr.t) (op : Operators.uopt)
    (texpr : E_Type.t) : E_Type.t =
  let typing_fun = T_Op.unop_typing_fun op in
  let tres = T_Op.type_op typing_fun [ texpr ] in
  match tres with
  | None ->
      let op_str = Operators.str_of_unopt op in
      T_Err.raise (T_Err.BadOp (op_str, [ texpr ])) ~cs:(T_Err.Expr expr)
  | Some tres' -> tres'

let type_binop (tctx : T_Ctx.t) (expr : E_Expr.t) (op : Operators.bopt)
    (texpr1 : E_Type.t) (texpr2 : E_Type.t) : E_Type.t =
  let typing_fun = T_Op.binop_typing_fun op in
  let tres = T_Op.type_op typing_fun [ texpr1; texpr2 ] in
  match tres with
  | None ->
      let op_str = Operators.str_of_binopt_single op in
      T_Err.raise
        (T_Err.BadOp (op_str, [ texpr1; texpr2 ]))
        ~cs:(T_Err.Expr expr)
  | Some tres' -> tres'

let type_ebinop (tctx : T_Ctx.t) (expr : E_Expr.t) (op : EOper.bopt)
    (texpr1 : E_Type.t) (texpr2 : E_Type.t) : E_Type.t =
  let typing_fun = T_Op.ebinop_typing_fun op in
  let tres = T_Op.type_op typing_fun [ texpr1; texpr2 ] in
  match tres with
  | None ->
      let op_str = EOper.str_of_binopt_single op in
      T_Err.raise
        (T_Err.BadOp (op_str, [ texpr1; texpr2 ]))
        ~cs:(T_Err.Expr expr)
  | Some tres' -> tres'

let type_triop (tctx : T_Ctx.t) (expr : E_Expr.t) (op : Operators.topt)
    (texpr1 : E_Type.t) (texpr2 : E_Type.t) (texpr3 : E_Type.t) : E_Type.t =
  let typing_fun = T_Op.triop_typing_fun op in
  let tres = T_Op.type_op typing_fun [ texpr1; texpr2; texpr3 ] in
  match tres with
  | None ->
      let op_str = Operators.str_of_triopt_single op in
      T_Err.raise
        (T_Err.BadOp (op_str, [ texpr1; texpr2; texpr3 ]))
        ~cs:(T_Err.Expr expr)
  | Some tres' -> tres'

let type_arg (tparam : E_Type.t) (arg : E_Expr.t) (targ : E_Type.t) : unit =
  let gerr_fun = Some (fun () -> T_Err.BadArgument (tparam, targ)) in
  T_Typing.test_typing_expr ~gerr_fun tparam arg targ

let type_fun_args (tctx : T_Ctx.t) (expr : E_Expr.t) (func : E_Func.t)
    (args : E_Expr.t list) (targs : E_Type.t list) : unit =
  let tparams = E_Func.get_params_t func in
  let nparams = List.length tparams in
  let nargs = List.length targs in
  if nparams != nargs then
    T_Err.raise (T_Err.MissingArgs (nparams, nargs)) ~cs:(T_Err.Expr expr)
  else
    let param_args = List.combine tparams (List.combine args targs) in
    List.iter
      (fun ((param, tparam), (arg, targ)) ->
        match tparam with
        | None -> ()
        | Some tparam' -> type_arg tparam' arg targ)
      param_args

let type_named_fun_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fname : string)
    (args : E_Expr.t list) (targs : E_Type.t list) : E_Type.t =
  let func = T_Ctx.get_func_by_name tctx fname in
  match func with
  | None -> T_Err.raise (T_Err.UnknownFunction fname) ~cs:(T_Err.Str fname)
  | Some func' ->
      let _ = type_fun_args tctx expr func' args targs in
      Option.default E_Type.AnyType (E_Func.get_return_t func')

let type_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fexpr : E_Expr.t)
    (args : E_Expr.t list) (targs : E_Type.t list) : E_Type.t =
  match fexpr with
  | E_Expr.Val (Val.Str fname) -> type_named_fun_call tctx expr fname args targs
  | default -> E_Type.AnyType

let type_newobj (tctx : T_Ctx.t) (tfes : (string * E_Type.t) list) : E_Type.t =
  let flds = Hashtbl.create !Config.default_hashtbl_sz in
  let _ =
    List.iter
      (fun (fn, ft) ->
        match Hashtbl.find_opt flds fn with
        | None -> Hashtbl.add flds fn { E_Type.t = ft; E_Type.opt = false }
        | Some _ -> T_Err.raise (T_Err.DuplicatedField fn) ~cs:(T_Err.Str fn))
      tfes
  in
  E_Type.ObjectType { E_Type.flds; E_Type.smry = None }

let type_lookup (tctx : T_Ctx.t) (oexpr : E_Expr.t) (tobj : E_Type.t)
    (fexpr : E_Expr.t) : E_Type.t =
  match (tobj, fexpr) with
  | E_Type.ObjectType tobj', E_Expr.Val (Val.Str fn) -> (
      match Hashtbl.find_opt (E_Type.get_obj_fields tobj') fn with
      | None -> T_Err.raise (T_Err.BadLookup (tobj, fn)) ~cs:(T_Err.Expr fexpr)
      | Some tfld -> E_Type.get_field_type tfld)
  | default ->
      T_Err.raise (T_Err.ExpectedObjectExpr oexpr) ~cs:(T_Err.Expr oexpr)

let rec type_expr (tctx : T_Ctx.t) (expr : E_Expr.t) : E_Type.t =
  match expr with
  | Val v -> type_val v
  | Var x -> type_var tctx x
  (* | GVar _ ->  *)
  | Const c -> type_const c
  | UnOpt (op, e) ->
      let te = type_expr tctx e in
      type_unop tctx expr op te
  | BinOpt (op, e1, e2) ->
      let te1 = type_expr tctx e1 in
      let te2 = type_expr tctx e2 in
      type_binop tctx expr op te1 te2
  | EBinOpt (op, e1, e2) ->
      let te1 = type_expr tctx e1 in
      let te2 = type_expr tctx e2 in
      type_ebinop tctx expr op te1 te2
  | TriOpt (op, e1, e2, e3) ->
      let te1 = type_expr tctx e1 in
      let te2 = type_expr tctx e2 in
      let te3 = type_expr tctx e3 in
      type_triop tctx expr op te1 te2 te3
  (* | NOpt (_, _) ->  *)
  | Call (fexpr, args, _) ->
      let targs = List.map (fun arg -> type_expr tctx arg) args in
      type_call tctx expr fexpr args targs
  (* | ECall (_, _) ->  *)
  | NewObj fes ->
      let tfes = List.map (fun (fn, ft) -> (fn, type_expr tctx ft)) fes in
      type_newobj tctx tfes
  | Lookup (oe, fe) ->
      let tobj = type_expr tctx oe in
      type_lookup tctx oe tobj fe
  (* | Curry (_, _) ->  *)
  (* | Symbolic (_, _) ->  *)
  | default -> E_Type.AnyType

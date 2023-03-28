open T_Err
open E_Expr

let type_val (expr : Val.t) : E_Type.t =
  match expr with
  | Val.Flt _ -> E_Type.NumberType
  | Val.Int _ -> E_Type.NumberType
  | Val.Str _ -> E_Type.StringType
  | Val.Bool _ -> E_Type.BooleanType
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
  try T_Typing.test_typing tparam targ
  with T_Err.TypeError terr -> (
    match terr.err with
    | T_Err.BadExpectedType (tref, texpr) ->
        T_Err.raise (T_Err.BadArgument (tparam, targ)) ~cs:(T_Err.Expr arg)
    | default -> ())

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

let type_fun_call (tctx : T_Ctx.t) (expr : E_Expr.t) (fexpr : E_Expr.t)
    (args : E_Expr.t list) (targs : E_Type.t list) : E_Type.t =
  match fexpr with
  | E_Expr.Val (Val.Str fname) -> type_named_fun_call tctx expr fname args targs
  | default -> E_Type.AnyType

let rec type_expr (tctx : T_Ctx.t) (expr : E_Expr.t) : E_Type.t =
  match expr with
  | Val v -> type_val v
  | Var var -> type_var tctx var
  (* | GVar _ ->  *)
  | Const const -> type_const const
  | UnOpt (op, expr') ->
      let texpr = type_expr tctx expr' in
      type_unop tctx expr op texpr
  | BinOpt (op, expr1, expr2) ->
      let texpr1 = type_expr tctx expr1 in
      let texpr2 = type_expr tctx expr2 in
      type_binop tctx expr op texpr1 texpr2
  | EBinOpt (op, expr1, expr2) ->
      let texpr1 = type_expr tctx expr1 in
      let texpr2 = type_expr tctx expr2 in
      type_ebinop tctx expr op texpr1 texpr2
  | TriOpt (op, expr1, expr2, expr3) ->
      let texpr1 = type_expr tctx expr1 in
      let texpr2 = type_expr tctx expr2 in
      let texpr3 = type_expr tctx expr3 in
      type_triop tctx expr op texpr1 texpr2 texpr3
  (* | NOpt (_, _) ->  *)
  | Call (fexpr, args, _) ->
      let targs = List.map (fun arg -> type_expr tctx arg) args in
      type_fun_call tctx expr fexpr args targs
  (* | ECall (_, _) ->  *)
  (* | NewObj _ ->  *)
  (* | Lookup (_, _) ->  *)
  (* | Curry (_, _) ->  *)
  (* | Symbolic (_, _) ->  *)
  | default -> E_Type.AnyType

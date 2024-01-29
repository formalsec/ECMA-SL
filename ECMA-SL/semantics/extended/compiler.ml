type c_expr = Stmt.t list * Expr.t
type c_stmt = Stmt.t list

module Const = struct
  let main_func = "main"
  let internal_esl_global = "___internal_esl_global"
end

open Source
open Operator

module Builder = struct
  let var = Utils.make_name_generator "__v"
  let global () : Expr.t = ?@(Expr.Var Const.internal_esl_global)

  let throw_checker (res : Id.t') (sthen : Stmt.t list) : Stmt.t =
    let iserror = ?@(Expr.UnOpt (TupleFirst, ?@(Expr.Var res))) in
    let value = ?@(Expr.UnOpt (TupleSecond, ?@(Expr.Var res))) in
    let selse = ?@(Stmt.Assign (?@res, value)) in
    ?@Stmt.(If (iserror, ?@(Block sthen), Some ?@(Block [ selse ])))

  let call_checker (res : Id.t') (ferr : Id.t option) : Stmt.t =
    let sreturn = ?@(Stmt.Return ?@(Expr.Var res)) in
    match ferr with
    | None -> throw_checker res [ sreturn ]
    | Some ferr' ->
      let ferr'' = Expr.Val (Val.Str ferr'.it) @> ferr'.at in
      let global = global () in
      let arg = ?@(Expr.UnOpt (TupleSecond, ?@(Expr.Var res))) in
      let shandler = ?@(Stmt.AssignCall (?@res, ferr'', [ global; arg ])) in
      let shandler_checker = throw_checker res [ sreturn ] in
      throw_checker res [ shandler; shandler_checker ]

  let else_block (ss : Stmt.t list) : Stmt.t option =
    match ss with [] -> None | ss -> Some ?@(Stmt.Block ss)

  let increment ?(n : int = 1) (x : Id.t') : Stmt.t =
    let inc = ?@(Expr.Val (Int n)) in
    ?@(Stmt.Assign (?@x, ?@(Expr.BinOpt (Plus, ?@(Expr.Var x), inc))))
end

let compile_sc_and (at : region) (res : Id.t') (e1_s : Stmt.t list)
  (e1_e : Expr.t) (e2_s : Stmt.t list) (e2_e : Expr.t) : c_expr =
  let etrue = ?@(Expr.Val (Val.Bool true)) in
  let efalse = ?@(Expr.Val (Val.Bool false)) in
  let e1_test = Expr.BinOpt (Operator.Eq, e1_e, efalse) @> e1_e.at in
  let e2_test = Expr.BinOpt (Operator.Eq, e2_e, efalse) @> e2_e.at in
  let res_true = ?@Stmt.(Block [ ?@(Assign (?@res, etrue)) ]) in
  let res_false = ?@Stmt.(Block [ ?@(Assign (?@res, efalse)) ]) in
  let inner_if = ?@Stmt.(If (e2_test, res_false, Some res_true)) in
  let inner_if' = ?@Stmt.(Block (e2_s @ [ inner_if ])) in
  let outer_if = ?@Stmt.(If (e1_test, res_false, Some inner_if')) in
  (e1_s @ [ outer_if ], Expr.Var res @> at)

let compile_sc_or (at : region) (res : Id.t') (e1_s : Stmt.t list)
  (e1_e : Expr.t) (e2_s : Stmt.t list) (e2_e : Expr.t) : c_expr =
  let etrue = ?@(Expr.Val (Val.Bool true)) in
  let efalse = ?@(Expr.Val (Val.Bool false)) in
  let e1_test = Expr.BinOpt (Operator.Eq, e1_e, etrue) @> e1_e.at in
  let e2_test = Expr.BinOpt (Operator.Eq, e2_e, etrue) @> e2_e.at in
  let res_true = ?@Stmt.(Block [ ?@(Assign (?@res, etrue)) ]) in
  let res_false = ?@Stmt.(Block [ ?@(Assign (?@res, efalse)) ]) in
  let inner_if = ?@Stmt.(If (e2_test, res_true, Some res_false)) in
  let inner_if' = ?@Stmt.(Block (e2_s @ [ inner_if ])) in
  let outer_if = ?@Stmt.(If (e1_test, res_true, Some inner_if')) in
  (e1_s @ [ outer_if ], Expr.Var res @> at)

let rec compile_expr (e : EExpr.t) : c_expr =
  match e.it with
  | Val x -> ([], Expr.Val x @> e.at)
  | Var x -> ([], Expr.Var x @> e.at)
  | GVar x -> compile_gvar e.at x
  | Const c -> compile_const e.at c
  | UnOpt (op, e') -> compile_unopt e.at op e'
  | BinOpt (op, e1, e2) -> compile_binopt e.at op e1 e2
  | TriOpt (op, e1, e2, e3) -> compile_triopt e.at op e1 e2 e3
  | NOpt (op, es) -> compile_nopt e.at op es
  | Call (fe, es, ferr) -> compile_call e.at fe es ferr
  | ECall (fn, es) -> compile_ecall e.at fn es
  | NewObj flds -> compile_newobj e.at flds
  | Lookup (oe, fe) -> compile_lookup e.at oe fe
  | Curry (fe, es) -> compile_curry e.at fe es
  | Symbolic (t, e') -> compile_symbolic e.at t e'

and compile_gvar (at : region) (x : Id.t') : c_expr =
  let res = Builder.var () in
  let global = Builder.global () in
  let entry = Expr.Val (Val.Str x) @> at in
  let sres = ?@(Stmt.FieldLookup (?@res, global, entry)) in
  ([ sres ], Expr.Var res @> at)

and compile_const (at : region) (c : const) : c_expr =
  match c with
  | MAX_VALUE -> ([], Expr.Val (Val.Flt Float.max_float) @> at)
  | MIN_VALUE -> ([], Expr.Val (Val.Flt 5e-324) @> at)
  | PI -> ([], Expr.Val (Val.Flt Float.pi) @> at)

and compile_unopt (at : region) (op : unopt) (e : EExpr.t) : c_expr =
  let (e_s, e_e) = compile_expr e in
  let res = Builder.var () in
  let sres =
    match op with
    | ObjectToList -> Stmt.AssignObjToList (?@res, e_e) @> at
    | _ -> ?@(Stmt.Assign (?@res, Expr.UnOpt (op, e_e) @> at))
  in
  (e_s @ [ sres ], Expr.Var res @> at)

and compile_binopt (at : region) (op : Operator.binopt) (e1 : EExpr.t)
  (e2 : EExpr.t) : c_expr =
  let (e1_s, e1_e) = compile_expr e1 in
  let (e2_s, e2_e) = compile_expr e2 in
  let res = Builder.var () in
  match op with
  | SCLogicalAnd -> compile_sc_and at res e1_s e1_e e2_s e2_e
  | SCLogicalOr -> compile_sc_or at res e1_s e1_e e2_s e2_e
  | _ ->
    let sres = ?@(Stmt.Assign (?@res, Expr.BinOpt (op, e1_e, e2_e) @> at)) in
    (e1_s @ e2_s @ [ sres ], Expr.Var res @> at)

and compile_triopt (at : region) (op : triopt) (e1 : EExpr.t) (e2 : EExpr.t)
  (e3 : EExpr.t) : c_expr =
  let (e1_s, e1_e) = compile_expr e1 in
  let (e2_s, e2_e) = compile_expr e2 in
  let (e3_s, e3_e) = compile_expr e3 in
  let res = Builder.var () in
  let op_e = Expr.TriOpt (op, e1_e, e2_e, e3_e) @> at in
  let sres = ?@(Stmt.Assign (?@res, op_e)) in
  (e1_s @ e2_s @ e3_s @ [ sres ], Expr.Var res @> at)

and compile_nopt (at : region) (op : nopt) (es : EExpr.t list) : c_expr =
  let (es_s, es_e) = List.map compile_expr es |> List.split in
  let res = Builder.var () in
  let sres = ?@(Stmt.Assign (?@res, Expr.NOpt (op, es_e) @> at)) in
  (List.concat es_s @ [ sres ], Expr.Var res @> at)

and compile_call (at : region) (fe : EExpr.t) (es : EExpr.t list)
  (ferr : Id.t option) : c_expr =
  let (fe_s, fe_e) = compile_expr fe in
  let (es_s, es_e) = List.map compile_expr es |> List.split in
  let global = Builder.global () in
  let res = Builder.var () in
  let sres = Stmt.AssignCall (?@res, fe_e, global :: es_e) @> at in
  let sthrow = Builder.call_checker res ferr in
  (fe_s @ List.concat es_s @ [ sres; sthrow ], Expr.Var res @> at)

and compile_ecall (at : region) (fn : Id.t) (es : EExpr.t list) : c_expr =
  let (es_s, es_e) = List.map compile_expr es |> List.split in
  let res = Builder.var () in
  let sres = Stmt.AssignECall (?@res, fn, es_e) @> at in
  (List.concat es_s @ [ sres ], Expr.Var res @> at)

and compile_newobj (at : region) (flds : (Id.t * EExpr.t) list) : c_expr =
  let build_fld res (fn, fe) =
    let (fe_s, fe_e) = compile_expr fe in
    let fn' = Expr.Val (Val.Str fn.it) @> fn.at in
    let sfassign = ?@(Stmt.FieldAssign (?@(Expr.Var res), fn', fe_e)) in
    fe_s @ [ sfassign ]
  in
  let res = Builder.var () in
  let snewobj = ?@(Stmt.AssignNewObj ?@res) in
  let sflds = List.map (build_fld res) flds in
  (snewobj :: List.concat sflds, Expr.Var res @> at)

and compile_lookup (at : region) (oe : EExpr.t) (fe : EExpr.t) : c_expr =
  let (oe_s, oe_e) = compile_expr oe in
  let (fe_s, fe_e) = compile_expr fe in
  let res = Builder.var () in
  let sres = Stmt.FieldLookup (?@res, oe_e, fe_e) @> at in
  (oe_s @ fe_s @ [ sres ], Expr.Var res @> at)

and compile_curry (at : region) (fe : EExpr.t) (es : EExpr.t list) : c_expr =
  let (fe_s, fe_e) = compile_expr fe in
  let (es_s, es_e) = List.map compile_expr es |> List.split in
  let res = Builder.var () in
  let sres = ?@(Stmt.Assign (?@res, Expr.Curry (fe_e, es_e) @> at)) in
  (fe_s @ List.concat es_s @ [ sres ], Expr.Var res @> at)

and compile_symbolic (at : region) (t : Type.t) (e : EExpr.t) : c_expr =
  let (e_s, e_e) = compile_expr e in
  let res = Builder.var () in
  let sres = ?@(Stmt.Assign (?@res, Expr.Symbolic (t, e_e) @> at)) in
  (e_s @ [ sres ], Expr.Var res @> at)

let compile_expr_of_stmt (e : EExpr.t) : c_expr =
  let (e_s, e_e) = compile_expr e in
  let e_s' = List.map (fun s -> { it = s.it; at = e.at }) e_s in
  (e_s', e_e)

let rec compile_stmt (s : EStmt.t) : c_stmt =
  match s.it with
  | Skip -> [ Stmt.Skip @> s.at ]
  | Debug s' -> compile_debug s.at s'
  | Block ss -> List.map compile_stmt ss |> List.concat
  | Print e -> compile_print s.at e
  | Return e -> compile_return s.at e
  | ExprStmt e -> fst (compile_expr_of_stmt e)
  | Assign (x, _, e) -> compile_assign s.at x e
  | GAssign (x, e) -> compile_gassign s.at x e
  | FieldAssign (oe, fe, e) -> compile_fieldassign s.at oe fe e
  | FieldDelete (oe, fe) -> compile_fielddelete s.at oe fe
  | If (ifcss, elsecs) -> compile_if ifcss elsecs
  | While (e, s') -> compile_while e s'
  | ForEach (x, e, s', _, _) -> compile_foreach x e s'
  | RepeatUntil (s', e, _) -> compile_repeatuntil s' e
  | Switch (e, css, dflt, _) -> compile_switch e css dflt
  | MatchWith (e, css) -> compile_matchwith e css
  | Lambda (x, id, _, ctxvars, _) -> compile_lambdacall s.at x id ctxvars
  | MacroApply (_, _) ->
    Eslerr.(internal __FUNCTION__ (UnexpectedEval (Some "MacroApply")))
  | Throw e -> compile_throw s.at e
  | Fail e -> compile_fail s.at e
  | Assert e -> compile_assert s.at e
  | Wrapper (_, s) -> compile_stmt s

and compile_debug (at : region) (s : EStmt.t) : c_stmt =
  match compile_stmt s with
  | [] -> Eslerr.(internal __FUNCTION__ (Expecting "non-empty statement list"))
  | s1_s :: ss_s -> (Stmt.Debug s1_s @> at) :: ss_s

and compile_print (at : region) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr_of_stmt e in
  e_s @ [ Stmt.Print e_e @> at ]

and compile_return (at : region) (e : EExpr.t) : c_stmt =
  let e' = if EExpr.isvoid e then ?@(EExpr.Val Val.Null) else e in
  let (e_s, e_e) = compile_expr_of_stmt e' in
  let ret = ?@(Expr.NOpt (TupleExpr, [ ?@(Expr.Val (Val.Bool false)); e_e ])) in
  e_s @ [ Stmt.Return ret @> at ]

and compile_assign (at : region) (x : Id.t) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr_of_stmt e in
  e_s @ [ Stmt.Assign (x, e_e) @> at ]

and compile_gassign (at : region) (x : Id.t) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr_of_stmt e in
  let global = Builder.global () in
  let entry = Expr.Val (Val.Str x.it) @> x.at in
  e_s @ [ Stmt.FieldAssign (global, entry, e_e) @> at ]

and compile_fieldassign (at : region) (oe : EExpr.t) (fe : EExpr.t) (e : EExpr.t)
  : c_stmt =
  let (oe_s, oe_e) = compile_expr_of_stmt oe in
  let (fe_s, fe_e) = compile_expr_of_stmt fe in
  let (e_s, e_e) = compile_expr_of_stmt e in
  oe_s @ fe_s @ e_s @ [ Stmt.FieldAssign (oe_e, fe_e, e_e) @> at ]

and compile_fielddelete (at : region) (oe : EExpr.t) (fe : EExpr.t) : c_stmt =
  let (oe_s, oe_e) = compile_expr_of_stmt oe in
  let (fe_s, fe_e) = compile_expr_of_stmt fe in
  oe_s @ fe_s @ [ Stmt.FieldDelete (oe_e, fe_e) @> at ]

and compile_if (ifcss : (EExpr.t * EStmt.t * EStmt.metadata_t list) list)
  (elsecs : (EStmt.t * EStmt.metadata_t list) option) : c_stmt =
  let compile_ifcs_f (e, s, _) acc =
    let (e_s, e_e) = compile_expr_of_stmt e in
    let sblock = Stmt.Block (compile_stmt s) @> s.at in
    e_s @ [ ?@(Stmt.If (e_e, sblock, Builder.else_block acc)) ]
  in
  List.fold_right compile_ifcs_f ifcss
    (Option.fold ~none:[] ~some:(fun (s, _) -> compile_stmt s) elsecs)

and compile_while (e : EExpr.t) (s : EStmt.t) : c_stmt =
  let (e_s, e_e) = compile_expr_of_stmt e in
  let sblock = Stmt.Block (compile_stmt s @ e_s) @> s.at in
  e_s @ [ ?@(Stmt.While (e_e, sblock)) ]

and compile_foreach (x : Id.t) (e : EExpr.t) (s : EStmt.t) : c_stmt =
  let i = Builder.var () in
  let len = Builder.var () in
  let guard = ?@(EExpr.BinOpt (Gt, ?@(EExpr.Var len), ?@(EExpr.Var i))) in
  let (e_s, e_e) = compile_expr_of_stmt e in
  let (guard_s, guard_e) = compile_expr guard in
  let s_s = compile_stmt s in
  let sinit = ?@(Stmt.Assign (?@i, ?@(Expr.Val (Int 0)))) in
  let slen = ?@(Stmt.Assign (?@len, ?@(Expr.UnOpt (ListLen, e_e)))) in
  let snth = ?@(Stmt.Assign (x, ?@Expr.(BinOpt (ListNth, e_e, ?@(Var i))))) in
  let sinc = Builder.increment i in
  let sblock = Stmt.Block ((snth :: s_s) @ (sinc :: guard_s)) @> s.at in
  e_s @ [ sinit; slen ] @ guard_s @ [ ?@(Stmt.While (guard_e, sblock)) ]

and compile_repeatuntil (s : EStmt.t) (e : EExpr.t option) : c_stmt =
  let e' = Option.value ~default:?@(EExpr.Val (Val.Bool false)) e in
  let s_s = compile_stmt s in
  let (e_s, e_e) = compile_expr_of_stmt e' in
  let guard = Expr.UnOpt (LogicalNot, e_e) @> e'.at in
  let block = s_s @ e_s in
  block @ [ ?@(Stmt.While (guard, Stmt.Block block @> s.at)) ]

and compile_switch (e : EExpr.t) (css : (EExpr.t * EStmt.t) list)
  (dflt : EStmt.t option) : c_stmt =
  let compile_cs_f e_e (ei, si) acc =
    let (ei_s, ei_e) = compile_expr_of_stmt ei in
    let guard = ?@(Expr.BinOpt (Eq, ei_e, e_e)) in
    let sblock = Stmt.Block (compile_stmt si) @> si.at in
    ei_s @ [ ?@(Stmt.If (guard, sblock, Builder.else_block acc)) ]
  in
  let (e_s, e_e) = compile_expr_of_stmt e in
  let dflt_s = Option.fold dflt ~none:[] ~some:compile_stmt in
  e_s @ List.fold_right (compile_cs_f e_e) css dflt_s

and compile_patv (e_e : Expr.t) (guard_var : Id.t') (pbn : Id.t) (pbv : EPat.pv)
  : c_stmt * Id.t' list * c_stmt =
  let pbn' = Expr.Val (Val.Str pbn.it) @> pbn.at in
  match pbv.it with
  | PatVar x -> ([], [], [ ?@(Stmt.FieldLookup (x @> pbv.at, e_e, pbn')) ])
  | PatVal v ->
    let fval = Builder.var () in
    let feq = Builder.var () in
    let feq' = ?@(Expr.BinOpt (Eq, ?@(Expr.Var fval), Expr.Val v @> pbv.at)) in
    let fval_s = ?@(Stmt.FieldLookup (?@fval, e_e, pbn')) in
    let feq_s = ?@(Stmt.Assign (?@feq, feq')) in
    ([ fval_s; feq_s ], [ feq ], [])
  | PatNone ->
    let fnone = ?@(Expr.UnOpt (LogicalNot, Expr.Var guard_var @> pbv.at)) in
    let fnone_s = ?@(Stmt.Assign (?@guard_var, fnone)) in
    ([ fnone_s ], [], [])

and compile_pat (e_e : Expr.t) (pat : EPat.t) : c_stmt * Expr.t * c_stmt =
  let compile_pbs_f (pre_s, guard_xs, pat_s) (pbn, pbv) =
    let guard_x = Builder.var () in
    let pbn' = Expr.Val (Val.Str pbn.it) @> pbn.at in
    let sinobj = ?@(Stmt.AssignInObjCheck (?@guard_x, pbn', e_e)) in
    let (pre_s', guard_xs', pat_s') = compile_patv e_e guard_x pbn pbv in
    ( pre_s @ (sinobj :: pre_s')
    , guard_xs @ (guard_x :: guard_xs')
    , pat_s @ pat_s' )
  in
  let etrue = ?@(Expr.Val (Val.Bool true)) in
  match pat.it with
  | DefaultPat -> ([], etrue, [])
  | ObjPat (pbs, _) ->
    let (pre_s, xs, pat_s) = List.fold_left compile_pbs_f ([], [], []) pbs in
    let guard_xs = List.map (fun x -> ?@(Expr.Var x)) xs in
    let guard_e = ?@(Expr.NOpt (NAryLogicalAnd, etrue :: guard_xs)) in
    (pre_s, guard_e, pat_s)

and compile_matchwith (e : EExpr.t) (css : (EPat.t * EStmt.t) list) : c_stmt =
  let compile_cs e_e (pat, s) acc =
    let (pre_s, guard_e, pat_s) = compile_pat e_e pat in
    let sblock = Stmt.Block (pat_s @ compile_stmt s) @> s.at in
    pre_s @ [ ?@(Stmt.If (guard_e, sblock, Builder.else_block acc)) ]
  in
  let (e_s, e_e) = compile_expr_of_stmt e in
  e_s @ List.fold_right (compile_cs e_e) css []

and compile_lambdacall (at : region) (x : Id.t) (id : string)
  (ctxvars : Id.t list) : c_stmt =
  let ctxvars' = List.map (fun x -> Expr.Var x.it @> x.at) ctxvars in
  let curry = ?@(Expr.Curry (?@(Expr.Val (Val.Str id)), ctxvars')) in
  [ Stmt.Assign (x, curry) @> at ]

and compile_throw (at : region) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr_of_stmt e in
  let ret = ?@(Expr.NOpt (TupleExpr, [ ?@(Expr.Val (Val.Bool true)); e_e ])) in
  e_s @ [ Stmt.Return ret @> at ]

and compile_fail (at : region) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr_of_stmt e in
  e_s @ [ Stmt.Fail e_e @> at ]

and compile_assert (at : region) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr_of_stmt e in
  e_s @ [ Stmt.Assert e_e @> at ]

(*
C(s) = s', _
---------------------------------------
C_f(function f (x1, ..., xn) { s }) =
   function f (___internal_esl_global, x1, ..., xn) { s' }


C(s) = s', _
---------------------------------------
C_f(function main () { s }) =
   function f () {
      ___internal_esl_global := {};
      s'
   }
*)
let compile_func (e_func : EFunc.t) : Func.t =
  let fname = EFunc.name e_func in
  let fparams = EFunc.params e_func in
  let fbody = EFunc.body e_func in
  let stmt_list = compile_stmt fbody in
  if fname.it = Const.main_func then
    let asgn_new_obj =
      Stmt.AssignNewObj (Const.internal_esl_global @> no_region) @> no_region
    in
    let stmt_list' = asgn_new_obj :: stmt_list in
    Func.create fname fparams (Stmt.Block stmt_list' @> no_region) @> no_region
  else
    let fparams' = (Const.internal_esl_global @> no_region) :: fparams in
    Func.create fname fparams' (Stmt.Block stmt_list @> no_region) @> no_region

let compile_lambda
  ((f_id, params, params', s) : string * Id.t list * Id.t list * EStmt.t) :
  Func.t =
  let stmt_list = compile_stmt s in
  let params'' =
    params @ [ Const.internal_esl_global @> no_region ] @ params'
  in
  Func.create (f_id @> no_region) params'' (Stmt.Block stmt_list @> no_region)
  @> no_region

let compile_prog (e_prog : EProg.t) : Prog.t =
  let funcs =
    List.fold_left
      (fun acc func -> acc @ [ compile_func func ])
      [] (EProg.funcs_lst e_prog)
  in
  let lambdas = EProg.lambdas e_prog in
  let lambda_funcs = List.map compile_lambda lambdas in
  Prog.create (lambda_funcs @ funcs)

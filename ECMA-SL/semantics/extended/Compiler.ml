open Val
open Expr
open Oper
open EOper
open E_Pat
open E_Pat_v
open E_Expr
open E_Stmt

let make_fresh_var_generator (pref : string) : unit -> string =
  let count = ref 0 in
  fun () ->
    let x = !count in
    count := x + 1;
    pref ^ string_of_int x

let __INTERNAL_ESL_GLOBAL__ = "___internal_esl_global"
let __MAIN_FUNC__ = "main"
let generate_fresh_var = make_fresh_var_generator "__v"

(*
  C(e1) = stmts_1, e_1'
  C(e2) = stmts_2, e_2'
  x fresh
------------------------
C(e1 &&& e2) =
    stmts_1;
    /* outer_if */
    if (e_1' = false) {
    	x := false
    } else {
        stmts_2;
        /* inner_if */
        if (e_2' = false) {
        	x := false
        } else {
        	x := true
        }
    }, x

*)
let compile_sc_and (x : string) (stmts_1 : Stmt.t list) (e1' : Expr.t)
    (stmts_2 : Stmt.t list) (e2' : Expr.t) : Stmt.t list * Expr.t =
  let inner_if =
    Stmt.If
      ( Expr.BinOpt (Oper.Equal, e2', Expr.Val (Val.Bool false)),
        Stmt.Assign (x, Expr.Val (Val.Bool false)),
        Some (Stmt.Assign (x, Expr.Val (Val.Bool true))) )
  in

  let outer_if =
    Stmt.If
      ( Expr.BinOpt (Oper.Equal, e1', Expr.Val (Val.Bool false)),
        Stmt.Assign (x, Expr.Val (Val.Bool false)),
        Some (Stmt.Block (stmts_2 @ [ inner_if ])) )
  in

  (stmts_1 @ [ outer_if ], Expr.Var x)

(*
  C(e1) = stmts_1, e_1'
  C(e2) = stmts_2, e_2'
  x fresh
------------------------
C(e1 ||| e2) =
    stmts_1;
    if (e_1' = true) {
    	x := true
    } else {
        stmts_2;
        if (e_2' = true) {
        	x := true
        } else {
        	x := false
        }
    }, x
*)
let compile_sc_or (x : string) (stmts_1 : Stmt.t list) (e1' : Expr.t)
    (stmts_2 : Stmt.t list) (e2' : Expr.t) : Stmt.t list * Expr.t =
  let inner_if =
    Stmt.If
      ( Expr.BinOpt (Oper.Equal, e2', Expr.Val (Val.Bool true)),
        Stmt.Assign (x, Expr.Val (Val.Bool true)),
        Some (Stmt.Assign (x, Expr.Val (Val.Bool false))) )
  in

  let outer_if =
    Stmt.If
      ( Expr.BinOpt (Oper.Equal, e1', Expr.Val (Val.Bool true)),
        Stmt.Assign (x, Expr.Val (Val.Bool true)),
        Some (Stmt.Block (stmts_2 @ [ inner_if ])) )
  in

  (stmts_1 @ [ outer_if ], Expr.Var x)

let compile_binopt (binop : Oper.bopt) ((stmts_1, e1) : Stmt.t list * Expr.t)
    ((stmts_2, e2) : Stmt.t list * Expr.t) : Stmt.t list * Expr.t =
  let var = generate_fresh_var () in
  ( stmts_1 @ stmts_2 @ [ Stmt.Assign (var, Expr.BinOpt (binop, e1, e2)) ],
    Expr.Var var )

let compile_triopt (triop : Oper.topt) ((stmts_1, e1) : Stmt.t list * Expr.t)
    ((stmts_2, e2) : Stmt.t list * Expr.t)
    ((stmts_3, e3) : Stmt.t list * Expr.t) : Stmt.t list * Expr.t =
  let var = generate_fresh_var () in
  ( stmts_1 @ stmts_2 @ stmts_3
    @ [ Stmt.Assign (var, Expr.TriOpt (triop, e1, e2, e3)) ],
    Expr.Var var )

(*
 y fresh
-------------------------
C_e(|x|) =
   y := ___internal_esl_global["x"], y
*)
let compile_gvar (x : string) : Stmt.t list * Expr.t =
  let y = generate_fresh_var () in
  let var =
    match x with
    (* Keep this in sync with Heap.ml function "str_with_global" *)
    | "global" -> Common.global_var_compiled
    | _ -> x
  in
  let f_lookup =
    Stmt.FieldLookup
      (y, Expr.Var __INTERNAL_ESL_GLOBAL__, Expr.Val (Val.Str var))
  in
  ([ f_lookup ], Expr.Var y)

(*
C_e(e) = stmts_e, x_e
----------------------
C(|x| := e) =
  stmts_e;
  ___internal_esl_global["x"] := x_e
*)
let compile_glob_assign (x : string) (stmts_e : Stmt.t list) (e : Expr.t) :
    Stmt.t list =
  let var =
    match x with
    (* Keep this in sync with Heap.ml function "str_with_global" *)
    | "global" -> Common.global_var_compiled
    | _ -> x
  in
  let f_asgn =
    Stmt.FieldAssign
      (Expr.Var __INTERNAL_ESL_GLOBAL__, Expr.Val (Val.Str var), e)
  in
  stmts_e @ [ f_asgn ]

(*
  C(x := lambda_{id_x} (xs) [ys] {s}) := x := {"id_x"}@(ys)
*)
let compile_lambda_call (x : string) (f : string) (ys : string list) :
    Stmt.t list =
  let e =
    Expr.Curry (Expr.Val (Val.Str f), List.map (fun v -> Expr.Var v) ys)
  in
  [ Stmt.Assign (x, e) ]

(*
C_e(e_f) = stmts_f, e_f'
C_e(e_i) = stmts_i, e_i' | i = 1, ..., n
x fresh
----------------------
C(e_f@(e1, ..., en)) =
   stmts_f;
   stmts_1;
   ...
   stmts_n;
   x := e_f'@(e1', ..., en'),
      x
*)
let compile_curry ((stmts_f, e_f) : Stmt.t list * Expr.t)
    (args : (Stmt.t list * Expr.t) list) : Stmt.t list * Expr.t =
  let x = generate_fresh_var () in
  let stmtss, es = List.split args in
  let stmts = List.concat stmtss in
  let stmts' = stmts_f @ stmts in
  let e_call = Expr.Curry (e_f, es) in
  let asgn = Stmt.Assign (x, e_call) in
  (stmts' @ [ asgn ], Expr.Var x)

(*
C(e) = stmts, e'
C(e_i) = stmts_i, e_i' | i=1, ..., n
C(s_i) = stmts_i' | i=1, ..., n
C(s') = stmts'
x_done fresh
-------------------------------------------------------------
   C(switch(e) { case e1: s1  case en: sn default: s'}) =
     stmts
      stmts_1
      if (e' = e_1') {
        stmts_1'
      } else {
        stmts_2
        if (e' = e_2') {
          stmts_2'
        } else {
          ...
          stmts'
        }
      }

   Compilation 2:
     stmts
      stmts_1
      if (e' = e_1' && ! x_done) {
        x_done := true;
        stmts_1'
      }
      stmts_2
      if (e' = e_2' && ! x_done) {
         x_done := true;
        stmts_2'
      }
      ...
      if (! x_done) {
        stmts'
      }
*)
let compile_switch (ret_e : Stmt.t list * Expr.t)
    (ret_cases : (Stmt.t list * Expr.t * Stmt.t list) list)
    (ret_so : Stmt.t list) : Stmt.t list =
  let stmts, e' = ret_e in
  let stmts'' =
    List.fold_right
      (fun (stmts_i, e_i', stmts_i') stmts_else ->
        let guard_i = Expr.BinOpt (Oper.Equal, e_i', e') in
        let stmt_if =
          Stmt.If (guard_i, Stmt.Block stmts_i', Some (Stmt.Block stmts_else))
        in
        stmts_i @ [ stmt_if ])
      ret_cases ret_so
  in
  stmts @ stmts''

let compile_fail (ret_e : Stmt.t list * Expr.t) : Stmt.t list =
  let stmts_expr, expr' = ret_e in
  stmts_expr @ [ Stmt.Fail expr' ]

let compile_throw (ret_e : Stmt.t list * Expr.t) : Stmt.t list =
  let stmts_expr, e' = ret_e in
  let ret_stmt =
    Stmt.Return (Expr.NOpt (Oper.TupleExpr, [ Expr.Val (Val.Bool true); e' ]))
  in
  stmts_expr @ [ ret_stmt ]

let compile_return (ret_e : Stmt.t list * Expr.t) : Stmt.t list =
  let stmts_expr, e' = ret_e in
  let ret_stmt =
    Stmt.Return (Expr.NOpt (Oper.TupleExpr, [ Expr.Val (Val.Bool false); e' ]))
  in
  stmts_expr @ [ ret_stmt ]

(*
C_e(e) = stmts', x'
C_e(e_i) = stmts_i, x_i
x fresh
-----------------------------------------
C_s({e}(e1, ..., en)) =
 	stmts';
    stmts_1;
    ...
    stmts_n;
    x := x' (___internal_esl_global, x_1, ..., x_n)
    if (first(x)) {
      return x
    } else {
      x := second(x)
    }, x


C_s({e}(e1, ..., en) catch g) =
 	stmts';
    stmts_1;
    ...
    stmts_n;
    x := x' (___internal_esl_global, x_1, ..., x_n)
    if (first(x)) {
      x := "g" (___internal_esl_global, second(x))
      if (first(x)) {
        return x
      } else {
        x := second (x)
      }
    } else {
      x := second(x)
    }, x


*)
let build_if_throw_basic (x : string) (s_then : Stmt.t) : Stmt.t =
  let guard = Expr.UnOpt (Oper.First, Expr.Var x) in
  let s_else = Stmt.Assign (x, Expr.UnOpt (Oper.Second, Expr.Var x)) in
  Stmt.If (guard, s_then, Some s_else)

let build_if_throw (x : string) (g : string option) : Stmt.t =
  match g with
  | None -> build_if_throw_basic x (Stmt.Return (Expr.Var x))
  | Some g ->
      let args =
        [
          Expr.Var __INTERNAL_ESL_GLOBAL__; Expr.UnOpt (Oper.Second, Expr.Var x);
        ]
      in
      let call_stmt = Stmt.AssignCall (x, Expr.Val (Val.Str g), args) in
      let inner_if = build_if_throw_basic x (Stmt.Return (Expr.Var x)) in
      build_if_throw_basic x (Stmt.Block [ call_stmt; inner_if ])

let compile_call (ret_f : Stmt.t list * Expr.t)
    (ret_args : (Stmt.t list * Expr.t) list) (g : string option) :
    Stmt.t list * Expr.t =
  let x = generate_fresh_var () in
  let fname_stmts, fname_expr = ret_f in
  let fargs_stmts_exprs = ret_args in
  let fargs_stmts, fargs_exprs = List.split fargs_stmts_exprs in
  let fargs_exprs' = Expr.Var __INTERNAL_ESL_GLOBAL__ :: fargs_exprs in
  let stmt_if = build_if_throw x g in
  ( fname_stmts @ List.concat fargs_stmts
    @ [ Stmt.AssignCall (x, fname_expr, fargs_exprs'); stmt_if ],
    Expr.Var x )

(*
C(e_i) = stmts_i, x_i ,e_i|i=1^n = es
x fresh
------------------------------
 C(extern f(es)) =
     stmts_i |i=1^n
     x := extern f(x_i|i=1^n), x
*)

let compile_e_call (f : string) (ret_args : (Stmt.t list * Expr.t) list) :
    Stmt.t list * Expr.t =
  let x = generate_fresh_var () in
  let fargs_stmts, fargs_exprs = List.split ret_args in
  ( List.concat fargs_stmts @ [ Stmt.AssignECall (x, f, fargs_exprs) ],
    Expr.Var x )

let compile_const (c : Oper.const) : Stmt.t list * Expr.t =
  match c with
  | MAX_VALUE -> ([], Expr.Val (Val.Flt Float.max_float))
  | MIN_VALUE -> ([], Expr.Val (Val.Flt 5e-324))
  | PI -> ([], Expr.Val (Val.Flt Float.pi))

let rec compile_ebinopt (binop : EOper.bopt) (e_e1 : E_Expr.t) (e_e2 : E_Expr.t)
    : Stmt.t list * Expr.t =
  let x = generate_fresh_var () in
  let stmts_1, e1 = compile_expr e_e1 in
  let stmts_2, e2 = compile_expr e_e2 in
  match binop with
  | SCLogAnd -> compile_sc_and x stmts_1 e1 stmts_2 e2
  | SCLogOr -> compile_sc_or x stmts_1 e1 stmts_2 e2

and compile_unopt (op : Oper.uopt) (expr : E_Expr.t) : Stmt.t list * Expr.t =
  let var = generate_fresh_var () in
  let stmts_expr, expr' = compile_expr expr in
  match op with
  | ObjToList ->
      (stmts_expr @ [ Stmt.AssignObjToList (var, expr') ], Expr.Var var)
  | _ ->
      (stmts_expr @ [ Stmt.Assign (var, Expr.UnOpt (op, expr')) ], Expr.Var var)

and compile_nopt (nop : Oper.nopt) (e_exprs : E_Expr.t list) :
    Stmt.t list * Expr.t =
  let var = generate_fresh_var () in
  let stmts_exprs = List.map compile_expr e_exprs in
  let stmts, exprs = List.split stmts_exprs in
  ( List.concat stmts @ [ Stmt.Assign (var, Expr.NOpt (nop, exprs)) ],
    Expr.Var var )

and compile_newobj (e_fes : (string * E_Expr.t) list) : Stmt.t list * Expr.t =
  let var = generate_fresh_var () in
  let newObj = Stmt.AssignNewObj var in
  let stmts =
    List.map
      (fun (pn, e) ->
        let stmts, e' = compile_expr e in
        stmts @ [ Stmt.FieldAssign (Expr.Var var, Expr.Val (Val.Str pn), e') ])
      e_fes
  in
  (newObj :: List.concat stmts, Expr.Var var)

and compile_lookup (expr : E_Expr.t) (field : E_Expr.t) : Stmt.t list * Expr.t =
  let var = generate_fresh_var () in
  let stmts_expr, expr' = compile_expr expr in
  let stmts_field, field' = compile_expr field in
  ( stmts_expr @ stmts_field @ [ Stmt.FieldLookup (var, expr', field') ],
    Expr.Var var )

and compile_assign (var : string) (e_exp : E_Expr.t) : Stmt.t list =
  let stmts, aux_var = compile_expr e_exp in
  stmts @ [ Stmt.Assign (var, aux_var) ]

and compile_block (e_stmts : E_Stmt.t list) : Stmt.t list =
  let stmts_lists = List.map compile_stmt e_stmts in
  List.concat stmts_lists

and compile_if (expr : E_Expr.t) (stmt1 : E_Stmt.t) (stmt2 : E_Stmt.t option) :
    Stmt.t list =
  let stmts_expr, expr' = compile_expr expr in
  let stmts_s1 = Stmt.Block (compile_stmt stmt1) in
  let stmts_s2 =
    match stmt2 with
    | None -> None
    | Some s2 -> Some (Stmt.Block (compile_stmt s2))
  in
  stmts_expr @ [ Stmt.If (expr', stmts_s1, stmts_s2) ]

and compile_while (expr : E_Expr.t) (stmt : E_Stmt.t) : Stmt.t list =
  let stmts_expr, expr' = compile_expr expr in
  let stmts_stmt = compile_stmt stmt in
  stmts_expr @ [ Stmt.While (expr', Stmt.Block (stmts_stmt @ stmts_expr)) ]

and compile_fieldassign (e_eo : E_Expr.t) (e_f : E_Expr.t) (e_ev : E_Expr.t) :
    Stmt.t list =
  let stmts_eo, expr_eo = compile_expr e_eo in
  let stmts_f, expr_f = compile_expr e_f in
  let stmts_ev, expr_ev = compile_expr e_ev in
  stmts_eo @ stmts_f @ stmts_ev
  @ [ Stmt.FieldAssign (expr_eo, expr_f, expr_ev) ]

and compile_fielddelete (expr : E_Expr.t) (field : E_Expr.t) : Stmt.t list =
  let stmts_expr, expr' = compile_expr expr in
  let stmts_field, field' = compile_expr field in
  stmts_expr @ stmts_field @ [ Stmt.FieldDelete (expr', field') ]

and compile_exprstmt (expr : E_Expr.t) : Stmt.t list =
  let stmts_expr, _ = compile_expr expr in
  stmts_expr

and compile_repeatuntil (stmt : E_Stmt.t) (expr : E_Expr.t) : Stmt.t list =
  let stmts_stmt = compile_stmt stmt in
  let stmts_expr, expr' = compile_expr expr in
  let not_expr = Expr.UnOpt (Oper.Not, expr') in
  let stmts = stmts_stmt @ stmts_expr in
  stmts @ [ Stmt.While (not_expr, Stmt.Block stmts) ]

and compile_patv (expr : Expr.t) (pname : string) (pat_v : E_Pat_v.t)
    (var_b : string) : string list * Stmt.t list * Stmt.t list =
  match pat_v with
  | PatVar v ->
      ([], [], [ Stmt.FieldLookup (v, expr, Expr.Val (Val.Str pname)) ])
  | PatVal v ->
      let b = generate_fresh_var () in
      let w = generate_fresh_var () in
      let stmt = Stmt.FieldLookup (w, expr, Expr.Val (Val.Str pname)) in
      let stmt_assign =
        Stmt.Assign (b, Expr.BinOpt (Oper.Equal, Expr.Var w, Expr.Val v))
      in
      ([ b ], [ stmt; stmt_assign ], [])
  | PatNone ->
      let stmt = Stmt.Assign (var_b, Expr.UnOpt (Oper.Not, Expr.Var var_b)) in
      ([], [ stmt ], [])

and compile_pn_pat (expr : Expr.t) ((pn, patv) : string * E_Pat_v.t) :
    string list * Stmt.t list * Stmt.t list =
  let fresh_b = generate_fresh_var () in
  let in_stmt = Stmt.AssignInObjCheck (fresh_b, Expr.Val (Val.Str pn), expr) in
  let bs, stmts, stmts' = compile_patv expr pn patv fresh_b in
  (fresh_b :: bs, in_stmt :: stmts, stmts')

and compile_pat (expr : Expr.t) (e_pat : E_Pat.t) :
    string list * Stmt.t list * Stmt.t list =
  match e_pat with
  | DefaultPat -> ([], [], [])
  | ObjPat (pn_pats, _) ->
      let bs, pre_stmts, in_stmts =
        List.fold_left
          (fun (bs, pre_stmts, in_stmts) pn_pat ->
            let bs', pre_stmts', in_stmts' = compile_pn_pat expr pn_pat in
            (bs @ bs', pre_stmts @ pre_stmts', in_stmts @ in_stmts'))
          ([], [], []) pn_pats
      in
      (bs, pre_stmts, in_stmts)

and compile_pats_stmts (expr : Expr.t) ((pat, stmt) : E_Pat.t * E_Stmt.t) :
    Stmt.t list * Expr.t * Stmt.t list =
  let bs, pre_pat_stmts, in_pat_stmts = compile_pat expr pat in
  let stmts = compile_stmt stmt in
  let and_bs =
    Expr.NOpt
      ( Oper.NAry_And,
        Expr.Val (Val.Bool true) :: List.map (fun b -> Expr.Var b) bs )
  in
  let if_stmt = in_pat_stmts @ stmts in
  (pre_pat_stmts, and_bs, if_stmt)

and compile_matchwith (expr : E_Expr.t) (pats_stmts : (E_Pat.t * E_Stmt.t) list)
    : Stmt.t list =
  let stmts_expr, expr' = compile_expr expr in
  let pat_stmts_bs_stmts_list =
    List.rev (List.map (compile_pats_stmts expr') pats_stmts)
  in
  let chained_ifs =
    match pat_stmts_bs_stmts_list with
    | [] -> []
    | (pat_stmts, bs_expr, stmts) :: rest ->
        let last_if =
          pat_stmts @ [ Stmt.If (bs_expr, Stmt.Block stmts, None) ]
        in
        List.fold_left
          (fun acc (ps, be, ss) ->
            ps @ [ Stmt.If (be, Stmt.Block ss, Some (Stmt.Block acc)) ])
          last_if rest
  in
  stmts_expr @ chained_ifs

and compile_expr (e_expr : E_Expr.t) : Stmt.t list * Expr.t =
  let c = compile_expr in
  let cs = List.map compile_expr in
  match e_expr with
  | Val x -> ([], Expr.Val x)
  | Var x -> ([], Expr.Var x)
  | GVar x -> compile_gvar x
  | Const c -> compile_const c
  | BinOpt (op, e1, e2) ->
      let stmts_1, e1' = compile_expr e1 in
      let stmts_2, e2' = compile_expr e2 in
      compile_binopt op (stmts_1, e1') (stmts_2, e2')
  | TriOpt (op, e1, e2, e3) ->
      let stmts_1, e1' = compile_expr e1 in
      let stmts_2, e2' = compile_expr e2 in
      let stmts_3, e3' = compile_expr e3 in
      compile_triopt op (stmts_1, e1') (stmts_2, e2') (stmts_3, e3')
  | EBinOpt (e_op, e_e1, e_e2) -> compile_ebinopt e_op e_e1 e_e2
  | UnOpt (op, e_e) -> compile_unopt op e_e
  | NOpt (op, e_es) -> compile_nopt op e_es
  | NewObj e_fes -> compile_newobj e_fes
  | Lookup (e_e, e_f) -> compile_lookup e_e e_f
  | Curry (f, es) -> compile_curry (c f) (cs es)
  | Call (f, e_es, g) ->
      let ret_f = compile_expr f in
      let ret_es = List.map compile_expr e_es in
      compile_call ret_f ret_es g
  | ECall (f, es) ->
      let ret_es = List.map compile_expr es in
      compile_e_call f ret_es

and compile_print (expr : E_Expr.t) : Stmt.t list =
  let stmts_expr, expr' = compile_expr expr in
  stmts_expr @ [ Stmt.Print expr' ]

and compile_assert (expr : E_Expr.t) : Stmt.t list =
  let stmts_expr, expr' = compile_expr expr in
  stmts_expr
  @ [
      Stmt.If
        ( Expr.UnOpt (Oper.Not, expr'),
          Stmt.Fail
            (Expr.Val
               (Oper.string_concat
                  (List [ Str "Assert failed: "; Str (E_Expr.str expr) ]))),
          None );
    ]

and compile_stmt (e_stmt : E_Stmt.t) : Stmt.t list =
  let compile_cases =
    List.map (fun (e, s) ->
        let stmts_e, e' = compile_expr e in
        let stmts_s = compile_stmt s in
        (stmts_e, e', stmts_s))
  in

  match e_stmt with
  | Skip -> [ Stmt.Skip ]
  | Print e -> compile_print e
  | Wrapper (_, s) -> compile_stmt s
  | Assign (v, e_exp) -> compile_assign v e_exp
  | GlobAssign (x, e) ->
      let stmts_e, e' = compile_expr e in
      compile_glob_assign x stmts_e e'
  | Block e_stmts -> compile_block e_stmts
  | If (e_e, e_s1, e_s2, _, _) -> compile_if e_e e_s1 e_s2
  | EIf (ifs, final_else) ->
      let acc =
        Option.map_default (fun (s, _) -> compile_stmt s) [] final_else
      in
      let ifs' = List.rev ifs in
      List.fold_left
        (fun acc (e, s, _) ->
          let stmts_e, e' = compile_expr e in
          let stmts_s = compile_stmt s in
          stmts_e @ [ Stmt.If (e', Stmt.Block stmts_s, Some (Stmt.Block acc)) ])
        acc ifs'
  | While (e_exp, e_s) -> compile_while e_exp e_s
  | ForEach (x, e_e, e_s, _, _) ->
      let len_str = generate_fresh_var () in
      let idx_str = generate_fresh_var () in
      let e_test = E_Expr.BinOpt (Oper.Gt, Var len_str, Var idx_str) in
      let stmt_inc =
        Stmt.Assign (idx_str, Expr.BinOpt (Oper.Plus, Var idx_str, Val (Int 1)))
      in
      let stmts_e_test, e_test' = compile_expr e_test in
      let stmts_e, e_e' = compile_expr e_e in
      let stmts_before =
        Stmt.Assign (idx_str, Val (Int 0))
        :: [ Stmt.Assign (len_str, Expr.UnOpt (Oper.ListLen, e_e')) ]
      in
      let stmt_assign_x =
        Stmt.Assign (x, Expr.BinOpt (Oper.Lnth, e_e', Var idx_str))
      in
      let stmts_s = compile_stmt e_s in
      stmts_e @ stmts_before @ stmts_e_test
      @ [
          Stmt.While
            ( e_test',
              Stmt.Block
                ((stmt_assign_x :: stmts_s) @ (stmt_inc :: stmts_e_test)) );
        ]
  | FieldAssign (e_eo, e_f, e_ev) -> compile_fieldassign e_eo e_f e_ev
  | FieldDelete (e_e, e_f) -> compile_fielddelete e_e e_f
  | ExprStmt e_e -> compile_exprstmt e_e
  | RepeatUntil (e_s, e_e, _) -> compile_repeatuntil e_s e_e
  | MatchWith (e_e, e_pats_e_stmts) -> compile_matchwith e_e e_pats_e_stmts
  | Assert e_e -> compile_assert e_e
  | Lambda (x, f, xs, ys, s) ->
      let ret = compile_lambda_call x f ys in
      ret
  | MacroApply (_, _) ->
      invalid_arg "Macros are not valid compilable statements."
  | Throw e_e ->
      let ret_e = compile_expr e_e in
      compile_throw ret_e
  | Fail e_e ->
      let ret_e = compile_expr e_e in
      compile_fail ret_e
  | Switch (e, cases, so, _) ->
      let ret_e = compile_expr e in
      let ret_cases = compile_cases cases in
      let ret_so = Option.map_default compile_stmt [] so in
      compile_switch ret_e ret_cases ret_so
  | Return e_e ->
      let ret_e = compile_expr e_e in
      compile_return ret_e

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
let compile_func (e_func : E_Func.t) : Func.t =
  let fname = E_Func.get_name e_func in
  let fparams = E_Func.get_params e_func in
  let fbody = E_Func.get_body e_func in
  let stmt_list = compile_stmt fbody in
  if fname = __MAIN_FUNC__ then
    let asgn_new_obj = Stmt.AssignNewObj __INTERNAL_ESL_GLOBAL__ in
    let stmt_list' = asgn_new_obj :: stmt_list in
    Func.create fname fparams (Stmt.Block stmt_list')
  else
    let fparams' = __INTERNAL_ESL_GLOBAL__ :: fparams in
    Func.create fname fparams' (Stmt.Block stmt_list)

let compile_lambda
    ((f_id, params, params', s) : string * string list * string list * E_Stmt.t)
    : Func.t =
  let stmt_list = compile_stmt s in
  let params'' = params @ [ __INTERNAL_ESL_GLOBAL__ ] @ params' in
  Func.create f_id params'' (Stmt.Block stmt_list)

let compile_prog (e_prog : E_Prog.t) : Prog.t =
  let funcs =
    List.fold_left
      (fun acc func -> acc @ [ compile_func func ])
      [] (E_Prog.get_funcs e_prog)
  in
  let lambdas = E_Prog.lambdas e_prog in
  let lambda_funcs = List.map compile_lambda lambdas in
  Prog.create (lambda_funcs @ funcs)

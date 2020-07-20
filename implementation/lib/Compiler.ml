let make_fresh_var_generator (pref : string) : (unit -> string) =
  let count = ref 0 in
  fun () -> let x = !count in
    count := x+1; pref ^ (string_of_int x)


let generate_fresh_var = make_fresh_var_generator "___temp"


let compile_val (e_val : E_Val.t) : Expr.t =
  let v = match e_val with
    | Flt e_f  -> Val.Flt e_f
    | Int e_i  -> Val.Int e_i
    | Bool e_b -> Val.Bool e_b
    | Str e_s  -> Val.Str e_s in
  Expr.Val (v)


let rec compile_binopt (e_op : E_Expr.bopt) (e_e1 : E_Expr.t) (e_e2 : E_Expr.t) : Stmt.t list * Expr.t =
  let op = match e_op with
    | Plus -> Expr.Plus
    | _    -> invalid_arg ("Exception in Compile.compile_binopt: Invalid e_op -> " ^ E_Expr.str_of_binopt e_op) in
  let stmts_1, e1 = compile_expr e_e1 in
  let stmts_2, e2 = compile_expr e_e2 in
  stmts_1 @ stmts_2, Expr.BinOpt (op, e1, e2)


and compile_nopt (e_nop : E_Expr.nopt) (e_exprs : E_Expr.t list) : Stmt.t list * Expr.t =
  let nop = match e_nop with
    | ListExpr -> Expr.ListExpr in
  let stmts_exprs = List.map compile_expr e_exprs in
  let stmts, exprs = List.split stmts_exprs in
  List.concat stmts, Expr.NOpt (nop, exprs)


and compile_call (fname : E_Expr.t) (fargs : E_Expr.t list) : Stmt.t list * Expr.t =
  let var = generate_fresh_var () in
  [Stmt.Call (var, (E_Expr.str fname), (List.map (fun arg -> snd (compile_expr arg)) fargs))], Expr.Var var


and compile_newobj (e_fes : (string * E_Expr.t) list) : Stmt.t list * Expr.t =
  let var = generate_fresh_var () in
  let newObj = Expr.AssignNewObj var in
  let stmts = List.map (fun (pn, e) -> let stmts, e' = compile_expr e in
                         stmts @ [Stmt.FieldAssign(Expr.Var var, Expr.Val (Val.Str pn), e')]) e_fes in
  List.concat stmts, Expr.Var var


and compile_assign (var : string) (e_exp : E_Expr.t) : Stmt.t list =
  let stmts, aux_var = compile_expr e_exp in
  stmts @ [Stmt.Assign (var, aux_var)]


and compile_fieldassign (e_eo : E_Expr.t) (e_f : E_Expr.t) (e_ev : E_Expr.t) : Stmt.t list =
  let e_o = snd (compile_expr e_eo) and
  f = snd (compile_expr e_f) and
  stmt, e_v = compile_expr e_ev in
  stmt @ [Stmt.FieldAssign (e_o, f, e_v)]


and compile_expr (e_expr : E_Expr.t) : Stmt.t list * Expr.t =
  match e_expr with
  | Val e_v                   -> [], compile_val e_v
  | Var e_v                   -> [], Expr.Var e_v
  | BinOpt (e_op, e_e1, e_e2) -> compile_binopt e_op e_e1 e_e2
  | UnOpt (op, e_e)           -> invalid_arg "Exception in Compile.compile_expr: UnOpt is not implemented"
  | NOpt (op, e_es)           -> compile_nopt op e_es
  | Call (f, e_es)            -> compile_call f e_es
  | NewObj (e_fes)            -> compile_newobj e_fes
  | Access (e_e, e_f)         -> invalid_arg "Exception in Compile.compile_expr: Access is not implemented"


let rec compile_stmt (e_stmt : E_Stmt.t) : Stmt.t list =
  match e_stmt with
  | Skip                            -> [Stmt.Skip]
  | Assign (v, e_exp)               -> compile_assign v e_exp
  | Seq (e_s1, e_s2)                -> compile_stmt e_s1 @ compile_stmt e_s2
  | If (e_exps_e_stmts)             -> invalid_arg "Exception in Compile.compile_stmt: If is not implemented"
  | While (e_exp, e_s)              -> invalid_arg "Exception in Compile.compile_stmt: While is not implemented"
  | Return e_exp                    -> invalid_arg "Exception in Compile.compile_stmt: Return is not implemented"
  | FieldAssign (e_eo, e_f, e_ev)   -> compile_fieldassign e_eo e_f e_ev
  | FieldDelete (e_e, e_f)          -> invalid_arg "Exception in Compile.compile_stmt: FieldDelete is not implemented"
  | ExprStmt e_e                    -> invalid_arg "Exception in Compile.compile_stmt: ExprStmt is not implemented"
  | RepeatUntil (e_s, e_e)          -> invalid_arg "Exception in Compile.compile_stmt: RepeatUntil is not implemented"
  | MatchWith (e_e, e_exps_e_stmts) -> invalid_arg "Exception in Compile.compile_stmt: MatchWith is not implemented"


let compile_func (e_func : E_Func.t) : Func.t =
  let fname = E_Func.get_name e_func and
    fparams = E_Func.get_params e_func and
    fbody = E_Func.get_body e_func in
  let stmt_list = compile_stmt fbody in
  Func.create fname fparams stmt_list


let compile_prog (e_prog : E_Prog.t) : Prog.t =
  let funcs = Hashtbl.fold (fun fname func acc -> acc @ [compile_func func]) e_prog [] in
  Prog.create funcs

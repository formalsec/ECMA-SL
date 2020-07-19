let generate_fresh_var (var : string) : Expr.t =
  match var with
  | "" -> Expr.Var "__temp"
  | _  -> Expr.Var (var ^ "'")


let rec stmt_to_list (stmt : Stmt.t) : Stmt.t list =
  match stmt with
  | Stmt.Seq (s1, s2) -> [s1] @ stmt_to_list s2
  | _                 -> [stmt]


let compile_val (e_val : E_Val.t) : Stmt.t * Expr.t =
  let v = match e_val with
    | Flt e_f  -> Val.Flt e_f
    | Int e_i  -> Val.Int e_i
    | Bool e_b -> Val.Bool e_b
    | Str e_s  -> Val.Str e_s in
  Stmt.Skip, Expr.Val (v)


let rec compile_binopt (e_op : E_Expr.bopt) (e_e1 : E_Expr.t) (e_e2 : E_Expr.t) : Stmt.t * Expr.t =
  let op = match e_op with
    | Plus -> Expr.Plus
    | _    -> invalid_arg ("Exception in Compile.compile_binopt: Invalid e_op -> " ^ E_Expr.str_of_binopt e_op) and
  e1 = snd (compile_expr e_e1 "") and
  e2 = snd (compile_expr e_e2 "") in
  Stmt.Skip, Expr.BinOpt (op, e1, e2)


and compile_newobj (e_fes : (string * E_Expr.t) list) (v : string) : Stmt.t * Expr.t =
  let var = generate_fresh_var v in
  let newObj = Stmt.Assign (Expr.str var, Expr.NewObj([])) in
  let stmt = List.fold_left (fun acc f_v -> Stmt.Seq(acc,
                                                     Stmt.FieldAssign(var,
                                                                      Expr.Val (Val.Str (fst f_v)),
                                                                      snd (compile_expr (snd f_v) v))
                                                    )
                            ) newObj e_fes in
  stmt, var


and compile_assign (var : string) (e_exp : E_Expr.t) : Stmt.t =
  let stmts, aux_var = compile_expr e_exp var in
  Stmt.Seq (stmts, Stmt.Assign (var, aux_var))


and compile_fieldassign (e_eo : E_Expr.t) (e_f : E_Expr.t) (e_ev : E_Expr.t) : Stmt.t =
  let e_o = snd (compile_expr e_eo "") and
  f = snd (compile_expr e_f "") and
  stmt, e_v = compile_expr e_ev "" in
  match stmt, e_v with
  | Stmt.Skip, _           -> Stmt.FieldAssign (e_o, f, e_v)
  | Stmt.Seq _, Expr.Var _ -> Stmt.Seq (stmt, Stmt.FieldAssign (e_o, f, e_v))
  | _                      -> invalid_arg ("Exception in Compile.compile_fieldassign: Invalid (stmt, e_v) pair -> " ^ Stmt.str stmt ^ ", " ^ Expr.str e_v)


and compile_expr (e_expr : E_Expr.t) (v : string) : Stmt.t * Expr.t =
  match e_expr with
  | Val e_v                   -> compile_val e_v
  | Var e_v                   -> Stmt.Skip, Expr.Var e_v
  | BinOpt (e_op, e_e1, e_e2) -> compile_binopt e_op e_e1 e_e2
  | UnOpt (op, e_e)           -> invalid_arg "Exception in Compile.compile_expr: UnOpt is not implemented"
  | NOpt (op, e_es)           -> invalid_arg "Exception in Compile.compile_expr: NOpt is not implemented"
  | Call (f, e_es)            -> invalid_arg "Exception in Compile.compile_expr: Call is not implemented"
  | NewObj (e_fes)            -> compile_newobj e_fes v
  | Access (e_e, e_f)         -> invalid_arg "Exception in Compile.compile_expr: Access is not implemented"


let rec compile_stmt (e_stmt : E_Stmt.t) : Stmt.t =
  match e_stmt with
  | Skip                            -> Stmt.Skip
  | Assign (v, e_exp)               -> compile_assign v e_exp
  | Seq (e_s1, e_s2)                -> Stmt.Seq (compile_stmt e_s1, compile_stmt e_s2)
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
  let stmt = compile_stmt fbody in
  let stmt_list = stmt_to_list stmt in
  Func.create fname fparams stmt_list


let compile_prog (e_prog : E_Prog.t) : Prog.t =
  let funcs = Hashtbl.fold (fun fname func acc -> acc @ [compile_func func]) e_prog [] in
  Prog.create funcs

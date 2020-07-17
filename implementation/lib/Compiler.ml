let compile_val (e_val : E_Val.t) : Val.t =
  match e_val with
  | Flt e_f  -> Val.Flt e_f
  | Int e_i  -> Val.Int e_i
  | Bool e_b -> Val.Bool e_b
  | Str e_s  -> Val.Str e_s

let compile_binopt (e_op : E_Expr.bopt) : Expr.bopt =
  match e_op with
  | Plus -> Expr.Plus
  | _    -> invalid_arg ("Exception in Compile.compile_binopt: Invalid e_op -> " ^ E_Expr.str_of_binopt e_op)

let rec compile_newobj (e_fes : (string * E_Expr.t) list) : Stmt.t =
  let var = Expr.Var "x" in
  let newObj = Stmt.Assign (Expr.str var, Expr.NewObj([])) in
  List.fold_left (fun acc f_v -> Stmt.Seq(acc,
                                          Stmt.FieldAssign(var,
                                                           Expr.Val (Val.Str (fst f_v)),
                                                           snd (compile_expr (snd f_v)))
                                         )
                 ) newObj e_fes

and compile_expr (e_expr : E_Expr.t) : Stmt.t * Expr.t =
  match e_expr with
  | Val e_v                   -> Stmt.Skip, Expr.Val (compile_val e_v)
  | Var e_v                   -> Stmt.Skip, Expr.Var e_v
  | BinOpt (e_op, e_e1, e_e2) -> (let op = compile_binopt e_op and
                                   e1 = snd (compile_expr e_e1) and
                                   e2 = snd (compile_expr e_e2) in
                                  Stmt.Skip, Expr.BinOpt (op, e1, e2))
  | UnOpt (op, e_e)           -> invalid_arg "Exception in Compile.compile_expr: UnOpt is not implemented"
  | NOpt (op, e_es)           -> invalid_arg "Exception in Compile.compile_expr: NOpt is not implemented"
  | Call (f, e_es)            -> invalid_arg "Exception in Compile.compile_expr: Call is not implemented"
  | NewObj (e_fes)            -> compile_newobj e_fes, Expr.Val (Val.Int 0)
  | Access (e_e, e_f)         -> invalid_arg "Exception in Compile.compile_expr: Access is not implemented"

let compile_stmt (e_stmt : E_Stmt.t) : Stmt.t =
  match e_stmt with
  | Skip                          -> Stmt.Skip
  | Assign (v, e_exp)             -> Stmt.Assign (v, snd (compile_expr e_exp))
  | Seq (e_s1, e_s2)              -> invalid_arg "Exception in Compile.compile_stmt: Seq is not implemented"
  | If (e_exps_e_stmts)           -> invalid_arg "Exception in Compile.compile_stmt: If is not implemented"
  | While (e_exp, e_s)            -> invalid_arg "Exception in Compile.compile_stmt: While is not implemented"
  | Return e_exp                  -> invalid_arg "Exception in Compile.compile_stmt: Return is not implemented"
  | FieldAssign (e_eo, e_f, e_ev) -> (let e_o = snd (compile_expr e_eo) and
                                       f = snd (compile_expr e_f) and
                                       e_v = snd (compile_expr e_ev) in
                                      Stmt.FieldAssign (e_o, f, e_v))
  | FieldDelete (e_e, e_f)        -> invalid_arg "Exception in Compile.compile_stmt: FieldDelete is not implemented"
  | ExprStmt e_e                  -> invalid_arg "Exception in Compile.compile_stmt: ExprStmt is not implemented"

let compile_func (e_func : E_Func.t) : Func.t =
  let fname = E_Func.get_name e_func and
    fparams = E_Func.get_params e_func and
    fbody = E_Func.get_body e_func in
  Func.create fname fparams (compile_stmt fbody)

let compile_prog (e_prog : E_Prog.t) : Prog.t =
  let funcs = Hashtbl.fold (fun fname func acc -> acc @ [compile_func func]) e_prog [] in
  Prog.create funcs

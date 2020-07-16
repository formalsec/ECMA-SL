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

let compile_stmt (e_stmt : E_Stmt.t) : E_Stmt.t = e_stmt

let compile_func (e_func : E_Func.t) : E_Func.t = e_func

let compile_prog (e_prog : E_Prog.t) : E_Prog.t = e_prog

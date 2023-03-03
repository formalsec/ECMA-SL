let type_function (func : E_Func.t) : unit =
  T_Stmt.type_stmt (E_Func.get_body func)

let type_program (prog : E_Prog.t) : unit =
  List.iter (fun f -> type_function f) (E_Prog.get_funcs prog)

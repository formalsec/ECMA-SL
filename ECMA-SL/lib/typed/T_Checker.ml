let type_function (tctx : T_Ctx.t) (func : E_Func.t) : unit =
  T_Stmt.type_stmt_res tctx (E_Func.get_body func)

let type_program (prog : E_Prog.t) : unit =
  let tctx = T_Ctx.create_typing_context prog in
  List.iter (fun f -> type_function tctx f) (E_Prog.get_funcs prog)

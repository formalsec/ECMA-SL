open T_Res
open E_Stmt
open Source

let type_stmt (tctx : T_Ctx.t) (stmt : E_Stmt.t) : T_Res.t =
  match stmt.it with
  | Skip -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Skip")
  | Fail _ -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Fail")
  | Throw _ -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Throw")
  | Print _ -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Print")
  | Assume _ -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Assume")
  | Assert _ -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Assert")
  | Return _ -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Return")
  | Wrapper (_, _) -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Wrapper")
  | Assign (_, _, _) -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Assign")
  | GlobAssign (_, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "GlobAssign")
  | Block _ -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "Block")
  | If (_, _, _, _, _) -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "If")
  | EIf (_, _) -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "EIf")
  | While (_, _) -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "While")
  | ForEach (_, _, _, _, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "ForEach")
  | FieldAssign (_, _, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "FieldAssign")
  | FieldDelete (_, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "FieldDelete")
  | ExprStmt _ -> T_Res.TypeError (T_Res.ErrMSg.not_implemented "ExprStmt")
  | RepeatUntil (_, _, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "RepeatUntil")
  | MatchWith (_, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "MatchWith")
  | MacroApply (_, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "MacroApply")
  | Switch (_, _, _, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "Switch")
  | Lambda (_, _, _, _, _) ->
      T_Res.TypeError (T_Res.ErrMSg.not_implemented "Lambda")

let type_stmt_res (tctx : T_Ctx.t) (stmt : E_Stmt.t) : unit =
  let res = type_stmt tctx stmt in
  match res with
  | Success -> ()
  | TypeError msg -> Printf.printf "%s" (T_Res.format_type_error stmt msg)

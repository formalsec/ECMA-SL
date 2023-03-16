open E_Stmt
open Source

let terr_catch (try_fun : unit -> 'a) (terrs : T_Err.t list) : T_Err.t list =
  try
    let _ = try_fun () in
    terrs
  with T_Err.TypeError terr -> List.append terrs [ terr ]

let terr_expr_catch (try_fun : unit -> E_Type.t) (terrs : T_Err.t list) :
    E_Type.t * T_Err.t list =
  try
    let texpr = try_fun () in
    (texpr, terrs)
  with T_Err.TypeError terr -> (E_Type.AnyType, List.append terrs [ terr ])

let terr_stmt_catch (try_fun : unit -> T_Err.t list) (terrs : T_Err.t list) :
    T_Err.t list =
  try
    let terrs' = try_fun () in
    List.append terrs terrs'
  with T_Err.TypeError terr -> List.append terrs [ terr ]

let type_assign (tctx : T_Ctx.t) (var : string) (tvar : E_Type.t option)
    (expr : E_Expr.t) : T_Err.t list =
  let texpr = T_Expr.type_expr tctx expr in
  let tvar' =
    match tvar with
    | None -> E_Type.AnyType (* TODO: Add type inference *)
    | Some tvar' ->
        if not (T_Typing.is_type_compatible tvar' texpr) then
          T_Ctx.terr_stmt tctx (T_Err.Expr expr)
            (T_Err.BadAssignment (tvar', texpr))
        else tvar'
  in
  let _ = T_Ctx.tenv_update tctx var tvar' in
  []

let type_return (tctx : T_Ctx.t) (expr : E_Expr.t) : T_Err.t list =
  let texpr = T_Expr.type_expr tctx expr in
  let tret = T_Ctx.get_return_t tctx in
  match tret with
  | None -> []
  | Some tret' ->
      if not (T_Typing.is_type_compatible tret' texpr) then
        T_Ctx.terr_stmt tctx (T_Err.Expr expr) (T_Err.BadReturn (tret', texpr))
      else []

let type_ifelse_while (tctx1 : T_Ctx.t) (expr : E_Expr.t) (stmt1 : E_Stmt.t)
    (stmt2 : E_Stmt.t option)
    (type_stmt_fun : T_Ctx.t -> E_Stmt.t -> T_Err.t list) : T_Err.t list =
  let terrs = [] in
  let aux_fun () = T_Expr.type_expr tctx1 expr in
  let texpr, terrs = terr_expr_catch aux_fun terrs in
  let terrs =
    if not (T_Typing.is_type_compatible E_Type.BooleanType texpr) then
      let aux_fun () =
        T_Ctx.terr_stmt tctx1 (T_Err.Expr expr)
          (T_Err.BadExpectedType (E_Type.BooleanType, texpr))
      in
      terr_catch aux_fun terrs
    else terrs
  in
  let tctx2 = T_Ctx.copy tctx1 in
  let terrs = terr_stmt_catch (fun () -> type_stmt_fun tctx1 stmt1) terrs in
  let terrs =
    match stmt2 with
    | None -> terrs
    | Some stmt2' ->
        terr_stmt_catch (fun () -> type_stmt_fun tctx2 stmt2') terrs
  in
  let _ = T_Ctx.tenv_intersect tctx1 tctx2 in
  terrs

let rec type_stmt (tctx : T_Ctx.t) (stmt : E_Stmt.t) : T_Err.t list =
  let _ = T_Ctx.set_stmt tctx stmt in
  let type_stmt' (stmt : E_Stmt.t) : T_Err.t list =
    match stmt.it with
    | Skip -> []
    (* | Fail _ -> [] *)
    (* | Throw _ -> [] *)
    | Print _ -> []
    (* | Assume _ -> [] *)
    (* | Assert _ -> [] *)
    | Return expr -> type_return tctx expr
    (* | Wrapper (_, _) -> [] *)
    | Assign (var, tvar, expr) -> type_assign tctx var tvar expr
    (* let texpr = T_Expr.type_expr tctx expr in
       match texpr with
       | T_Expr.Val texpr' -> type_assign tctx stmt x tvar expr texpr'
       | T_Expr.Err err -> [ T_Err.terr (T_Err.Stmt stmt) (T_Err.Stmt stmt) err ]) *)
    (* | GlobAssign (_, _) -> [] *)
    | Block stmts ->
        List.concat (List.map (fun stmt -> type_stmt tctx stmt) stmts)
    | If (expr, stmt1, stmt2, _, _) ->
        type_ifelse_while tctx expr stmt1 stmt2 type_stmt
    (* | EIf (_, _) -> [] *)
    | While (expr, stmt) -> type_ifelse_while tctx expr stmt None type_stmt
    (* | ForEach (_, _, _, _, _) -> [] *)
    (* | FieldAssign (_, _, _) -> [] *)
    (* | FieldDelete (_, _) -> [] *)
    | ExprStmt expr ->
        let _ = T_Expr.type_expr tctx expr in
        []
    (* | RepeatUntil (_, _, _) -> [] *)
    (* | MatchWith (_, _) -> [] *)
    (* | MacroApply (_, _) -> [] *)
    (* | Switch (_, _, _, _) -> [] *)
    (* | Lambda (_, _, _, _, _) -> [] *)
    | default -> []
  in
  try type_stmt' stmt with T_Err.TypeError terr -> [ terr ]

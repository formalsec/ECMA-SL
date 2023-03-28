open T_Err
open E_Stmt
open Source

let terr_none () : T_Err.t list = []

let terr_stmt (terrs : T_Err.t list) (stmt : E_Stmt.t) : T_Err.t list =
  List.map
    (fun terr ->
      if terr.source = T_Err.NoSource then
        T_Err.create terr.err ~src:(T_Err.Stmt stmt) ~cs:terr.cause
      else terr)
    terrs

let terr_catch_expr (try_fun : unit -> E_Type.t) (terrs : T_Err.t list) :
    E_Type.t * T_Err.t list =
  try
    let texpr = try_fun () in
    (texpr, terrs)
  with T_Err.TypeError terr -> (E_Type.AnyType, List.append terrs [ terr ])

let terr_catch_stmt (try_fun : unit -> T_Err.t list) (terrs : T_Err.t list) :
    T_Err.t list =
  try
    let terrs' = try_fun () in
    List.append terrs terrs'
  with T_Err.TypeError terr -> List.append terrs [ terr ]

let terr_catch_assign_return (tref : E_Type.t) (expr : E_Expr.t)
    (texpr : E_Type.t) (err_fun : E_Type.t * E_Type.t -> T_Err.err) : unit =
  try T_Typing.test_typing tref texpr
  with TypeError terr -> (
    match terr.err with
    | T_Err.BadExpectedType (tref', texpr') ->
        T_Err.raise (err_fun (tref', texpr')) ~cs:(T_Err.Expr expr)
    | default -> ())

let type_assign (tctx : T_Ctx.t) (var : string) (tvar : E_Type.t option)
    (expr : E_Expr.t) : unit =
  let texpr = T_Expr.type_expr tctx expr in
  let tvar' =
    match tvar with
    | None -> E_Type.AnyType (* TODO: Add type inference *)
    | Some tvar' ->
        let err_fun (tref, texpr) = T_Err.BadAssignment (tref, texpr) in
        let _ = terr_catch_assign_return tvar' expr texpr err_fun in
        tvar'
  in
  T_Ctx.tenv_update tctx var tvar'

let type_return (tctx : T_Ctx.t) (expr : E_Expr.t) : unit =
  let texpr = T_Expr.type_expr tctx expr in
  let tret = T_Ctx.get_return_t tctx in
  match tret with
  | None -> () (* TODO: Add type inference *)
  | Some tret' ->
      let err_fun (tref, texpr) = T_Err.BadReturn (tref, texpr) in
      terr_catch_assign_return tret' expr texpr err_fun

let type_ifelse_while (tctx : T_Ctx.t) (expr : E_Expr.t) (stmt1 : E_Stmt.t)
    (stmt2 : E_Stmt.t option)
    (type_stmt_fun : T_Ctx.t -> E_Stmt.t -> T_Err.t list) : T_Err.t list =
  let terrs = [] in
  let if_guard_typing_fun () = T_Expr.type_expr tctx expr in
  let texpr, terrs = terr_catch_expr if_guard_typing_fun terrs in
  let terrs =
    if not (T_Typing.is_typeable E_Type.BooleanType texpr) then
      let err = T_Err.BadExpectedType (E_Type.BooleanType, texpr) in
      List.append terrs [ T_Err.create err ~cs:(T_Err.Expr expr) ]
    else terrs
  in
  let tctx2 = T_Ctx.copy tctx in
  let terrs = terr_catch_stmt (fun () -> type_stmt_fun tctx stmt1) terrs in
  let terrs =
    match stmt2 with
    | None -> terrs
    | Some stmt2' ->
        terr_catch_stmt (fun () -> type_stmt_fun tctx2 stmt2') terrs
  in
  let _ = T_Ctx.tenv_intersect tctx tctx2 in
  terrs

let rec type_stmt (tctx : T_Ctx.t) (stmt : E_Stmt.t) : T_Err.t list =
  let type_stmt' (stmt : E_Stmt.t) : T_Err.t list =
    match stmt.it with
    | Skip -> terr_none ()
    (* | Fail _ -> [] *)
    (* | Throw _ -> [] *)
    | Print _ -> terr_none ()
    (* | Assume _ -> [] *)
    (* | Assert _ -> [] *)
    | Return expr -> terr_none (type_return tctx expr)
    (* | Wrapper (_, _) -> [] *)
    | Assign (var, tvar, expr) -> terr_none (type_assign tctx var tvar expr)
    (* | GlobAssign (_, _) -> [] *)
    | Block stmts ->
        List.concat (List.map (fun stmt -> type_stmt tctx stmt) stmts)
    | If (expr, stmt1, stmt2, _, _) ->
        terr_stmt (type_ifelse_while tctx expr stmt1 stmt2 type_stmt) stmt
    (* | EIf (_, _) -> [] *)
    | While (expr, stmt) ->
        terr_stmt (type_ifelse_while tctx expr stmt None type_stmt) stmt
    (* | ForEach (_, _, _, _, _) -> [] *)
    (* | FieldAssign (_, _, _) -> [] *)
    (* | FieldDelete (_, _) -> [] *)
    | ExprStmt expr -> terr_none (ignore (T_Expr.type_expr tctx expr))
    (* | RepeatUntil (_, _, _) -> [] *)
    (* | MatchWith (_, _) -> [] *)
    (* | MacroApply (_, _) -> [] *)
    (* | Switch (_, _, _, _) -> [] *)
    (* | Lambda (_, _, _, _, _) -> [] *)
    | default -> terr_none ()
  in
  try type_stmt' stmt
  with T_Err.TypeError terr ->
    [ T_Err.create terr.err ~src:(T_Err.Stmt stmt) ~cs:terr.cause ]

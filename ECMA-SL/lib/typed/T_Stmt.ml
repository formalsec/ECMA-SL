open E_Stmt
open Source
open T_Err

let terr_none () : T_Err.t list = []

let terr_stmt (terrs : T_Err.t list) (stmt : E_Stmt.t) : T_Err.t list =
  List.map
    (fun terr ->
      if terr.src = T_Err.NoSource then
        T_Err.create terr.err ~src:(T_Err.Stmt stmt) ~cs:terr.cs
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
  try List.append terrs (try_fun ())
  with T_Err.TypeError terr -> List.append terrs [ terr ]

let type_assign (tctx : T_Ctx.t) (var : string) (tvar : E_Type.t option)
    (expr : E_Expr.t) : unit =
  let texpr = T_Expr.type_expr tctx expr in
  let treal =
    match tvar with
    | None -> E_Type.AnyType (* TODO: Add type inference *)
    | Some tvar' ->
        let gerr_fun = Some (fun () -> T_Err.BadAssignment (tvar', texpr)) in
        let _ = T_Typing.test_typing_expr ~gerr_fun tvar' expr texpr in
        tvar'
  in
  T_Ctx.tref_update tctx var tvar;
  T_Ctx.tenv_update tctx var treal

let type_return (tctx : T_Ctx.t) (expr : E_Expr.t) : unit =
  let texpr = T_Expr.type_expr tctx expr in
  let tret = T_Ctx.get_return_t tctx in
  match tret with
  | None -> () (* TODO: Add type inference *)
  | Some tret' ->
      let gerr_fun = Some (fun () -> T_Err.BadReturn (tret', texpr)) in
      T_Typing.test_typing_expr ~gerr_fun tret' expr texpr

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
  let _ = T_Ctx.trefenv_intersect tctx tctx2 in
  terrs

let type_fassign (tctx : T_Ctx.t) (oexpr : E_Expr.t) (fexpr : E_Expr.t)
    (expr : E_Expr.t) : unit =
  let texpr = T_Expr.type_expr tctx expr in
  let tobj = T_Expr.type_expr tctx oexpr in
  match (tobj, fexpr) with
  | E_Type.ObjectType tobj', E_Expr.Val (Val.Str fn) -> (
      match Hashtbl.find_opt (E_Type.get_obj_fields tobj') fn with
      | None -> T_Err.raise (T_Err.BadLookup (tobj, fn)) ~cs:(T_Err.Expr fexpr)
      | Some tfld ->
          let tfld' = E_Type.get_field_type tfld in
          let gerr_fun = Some (fun () -> T_Err.BadAssignment (tfld', texpr)) in
          T_Typing.test_typing_expr ~gerr_fun tfld' expr texpr)
  | default ->
      T_Err.raise (T_Err.ExpectedObjectExpr oexpr) ~cs:(T_Err.Expr oexpr)

let rec type_stmt (tctx : T_Ctx.t) (stmt : E_Stmt.t) : T_Err.t list =
  let type_stmt' (stmt : E_Stmt.t) : T_Err.t list =
    match stmt.it with
    | Skip -> terr_none ()
    (* | Fail _ -> [] *)
    (* | Throw _ -> [] *)
    | Print _ -> terr_none ()
    (* | Assume _ -> [] *)
    (* | Assert _ -> [] *)
    | Return e -> terr_none (type_return tctx e)
    (* | Wrapper (_, _) -> [] *)
    | Assign (x, t, e) -> terr_none (type_assign tctx x t e)
    (* | GlobAssign (_, _) -> [] *)
    | Block stmts ->
        List.concat (List.map (fun stmt -> type_stmt tctx stmt) stmts)
    | If (e, s1, s2, _, _) ->
        terr_stmt (type_ifelse_while tctx e s1 s2 type_stmt) stmt
    (* | EIf (_, _) -> [] *)
    | While (e, s) -> terr_stmt (type_ifelse_while tctx e s None type_stmt) stmt
    (* | ForEach (_, _, _, _, _) -> [] *)
    | FieldAssign (oe, fe, e) -> terr_none (type_fassign tctx oe fe e)
    (* | FieldDelete (_, _) -> [] *)
    | ExprStmt e -> terr_none (ignore (T_Expr.type_expr tctx e))
    (* | RepeatUntil (_, _, _) -> [] *)
    (* | MatchWith (_, _) -> [] *)
    (* | MacroApply (_, _) -> [] *)
    (* | Switch (_, _, _, _) -> [] *)
    (* | Lambda (_, _, _, _, _) -> [] *)
    | default -> terr_none ()
  in
  try type_stmt' stmt
  with T_Err.TypeError terr ->
    [ T_Err.create terr.err ~src:(T_Err.Stmt stmt) ~cs:terr.cs ]

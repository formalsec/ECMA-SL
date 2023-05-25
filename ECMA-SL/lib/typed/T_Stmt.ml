open E_Stmt
open Source
open T_Err

let terr_none () : T_Err.t list = []

let catch_terr_stmt (tctx : T_Ctx.t) (tstmt_fun : unit -> unit) : T_Err.t list =
  try tstmt_fun () |> fun () -> []
  with TypeError terr ->
    [ { terr with src = T_Err.Stmt (T_Ctx.get_stmt tctx) } ]

let catch_terr_expr (tctx : T_Ctx.t) (texpr_fun : unit -> T_Expr.texpr_t) :
    T_Expr.texpr_t * T_Err.t list =
  try texpr_fun () |> fun texpr -> (texpr, [])
  with T_Err.TypeError terr ->
    let terr' = { terr with src = T_Err.Stmt (T_Ctx.get_stmt tctx) } in
    ((E_Type.AnyType, E_Type.AnyType), [ terr' ])

let type_assign (tctx : T_Ctx.t) (var : string) (tvar : E_Type.t option)
    (expr : E_Expr.t) : unit =
  let tdefault = T_Ctx.tvar_default E_Type.UnknownType in
  let tprev = Option.default tdefault (T_Ctx.tenv_find tctx var) in
  let rtprev, mut = (T_Ctx.tvar_tref tprev, T_Ctx.tvar_mut tprev) in
  let tref = Option.default (T_Ctx.tvar_tref tprev) tvar in
  let rtexpr, ntexpr = T_Expr.type_expr tctx expr in
  let _ = T_Typing.type_check expr tref (rtexpr, ntexpr) in
  if mut || rtprev = tref then
    T_Ctx.tvar_create tref ntexpr mut |> fun tvar ->
    T_Ctx.tenv_update tctx var tvar
  else T_Err.raise (T_Err.BadTypeUpdate (rtprev, tref))

let type_return (tctx : T_Ctx.t) (expr : E_Expr.t) : unit =
  let tret = Option.default E_Type.AnyType (T_Ctx.get_return_t tctx) in
  let texpr = T_Expr.type_expr tctx expr in
  try ignore (T_Typing.type_check expr tret texpr)
  with T_Err.TypeError terr -> (
    match terr.T_Err.errs with
    | T_Err.BadValue (tref, texpr) :: _ ->
        T_Err.update terr (T_Err.BadReturn (tref, texpr))
    | _ -> failwith "Typed ECMA-SL: T_Stmt.type_return")

let type_guard (tctx : T_Ctx.t) (expr : E_Expr.t) (texpr : T_Expr.texpr_t) :
    T_Err.t list =
  try T_Typing.type_check expr E_Type.BooleanType texpr |> fun () -> []
  with T_Err.TypeError terr -> (
    match terr.T_Err.errs with
    | T_Err.BadValue (tref, texpr) :: _ ->
        catch_terr_stmt tctx (fun () ->
            T_Err.update terr (T_Err.BadExpectedType (tref, texpr)))
    | _ -> failwith "Typed ECMA-SL: T_Stmt.type_ifelse_while")

let apply_constrains (tctx : T_Ctx.t) (form : T_Constraint.t) : T_Err.t list =
  try T_Constraint.apply tctx form |> fun () -> []
  with T_Err.TypeError terr -> (
    match terr.T_Err.errs with
    | T_Err.NoOverlapComp _ :: _ ->
        catch_terr_stmt tctx (fun () -> T_Err.continue terr)
    | _ -> [])

let type_ifelse (tctx : T_Ctx.t) (expr : E_Expr.t) (stmt1 : E_Stmt.t)
    (stmt2 : E_Stmt.t option)
    (type_stmt_fun : T_Ctx.t -> E_Stmt.t -> T_Err.t list) : T_Err.t list =
  let texpr_fun () = T_Expr.type_expr tctx expr in
  let texpr, terr_expr = catch_terr_expr tctx texpr_fun in
  let stmt2 = Option.default (Skip @@ no_region) stmt2 in
  let terrs_guard = type_guard tctx expr texpr in
  let tctx1, tctx2 = (T_Ctx.copy tctx, T_Ctx.copy tctx) in
  let form1 = T_Constraint.generate tctx expr in
  let form2 =
    if stmt2 <> Skip @@ no_region then T_Constraint.Not form1
    else T_Constraint.NoConstraint
  in
  let terr_form1 = apply_constrains tctx1 form1 in
  let terr_form2 = apply_constrains tctx2 form2 in
  let terrs_stmt1 = type_stmt_fun tctx1 stmt1 in
  let terrs_stmt2 = type_stmt_fun tctx2 stmt2 in
  let _ = T_Ctx.tenv_intersect tctx [ tctx1; tctx2 ] in
  List.concat
    [ terr_expr; terrs_guard; terr_form1; terr_form2; terrs_stmt1; terrs_stmt2 ]

let type_while (tctx : T_Ctx.t) (expr : E_Expr.t) (stmt : E_Stmt.t)
    (type_stmt_fun : T_Ctx.t -> E_Stmt.t -> T_Err.t list) : T_Err.t list =
  let texpr_fun () = T_Expr.type_expr tctx expr in
  let texpr, terr_expr = catch_terr_expr tctx texpr_fun in
  let terrs_guard = type_guard tctx expr texpr in
  let _ = T_Ctx.tenv_reset_narrowing tctx in
  let tctx' = T_Ctx.tenv_lock_types (T_Ctx.copy tctx) in
  let form = T_Constraint.generate tctx' expr in
  let terr_form = apply_constrains tctx' form in
  let terrs_stmt = type_stmt_fun tctx' stmt in
  List.concat [ terr_expr; terrs_guard; terr_form; terrs_stmt ]

let type_fassign (tctx : T_Ctx.t) (oexpr : E_Expr.t) (fexpr : E_Expr.t)
    (expr : E_Expr.t) : unit =
  let rt_of_nt rtoexpr nt =
    match rtoexpr with
    | E_Type.UnionType rts -> List.find (fun rt -> rt = nt) rts
    | _ -> rtoexpr
  in
  let type_fassign' texpr fn rtoexpr nt =
    let tref = T_Expr.type_fld_lookup oexpr fexpr fn (rt_of_nt rtoexpr nt) in
    T_Typing.type_check expr tref texpr
  in
  let texpr = T_Expr.type_expr tctx expr in
  let rtoexpr, ntoexpr = T_Expr.type_expr tctx oexpr in
  match (fexpr, ntoexpr) with
  | E_Expr.Val (Val.Str fn), E_Type.UnionType nts ->
      List.iter (type_fassign' texpr fn rtoexpr) nts
  | E_Expr.Val (Val.Str fn), _ ->
      List.iter (type_fassign' texpr fn rtoexpr) [ ntoexpr ]
  | _ -> ()

let rec type_stmt (tctx : T_Ctx.t) (stmt : E_Stmt.t) : T_Err.t list =
  let _ = T_Ctx.set_stmt tctx stmt in
  match stmt.it with
  | Skip -> terr_none ()
  (* | Fail _ -> [] *)
  (* | Throw _ -> [] *)
  | Print _ -> terr_none ()
  (* | Assume _ -> [] *)
  (* | Assert _ -> [] *)
  | Return e ->
      let return_fun () = type_return tctx e in
      catch_terr_stmt tctx return_fun
  (* | Wrapper (_, _) -> [] *)
  | Assign (x, t, e) ->
      let assign_fun () = type_assign tctx x t e in
      catch_terr_stmt tctx assign_fun
  (* | GlobAssign (_, _) -> [] *)
  | Block stmts ->
      List.concat (List.map (fun stmt -> type_stmt tctx stmt) stmts)
  | If (e, s1, s2, _, _) -> type_ifelse tctx e s1 s2 type_stmt
  (* | EIf (_, _) -> [] *)
  | While (e, s) -> type_while tctx e s type_stmt
  (* | ForEach (_, _, _, _, _) -> [] *)
  | FieldAssign (oe, fe, e) ->
      let fassign_fun () = type_fassign tctx oe fe e in
      catch_terr_stmt tctx fassign_fun
  (* | FieldDelete (_, _) -> [] *)
  | ExprStmt e ->
      let expr_fun () = ignore (T_Expr.type_expr tctx e) in
      catch_terr_stmt tctx expr_fun
  (* | RepeatUntil (_, _, _) -> [] *)
  (* | MatchWith (_, _) -> [] *)
  (* | MacroApply (_, _) -> [] *)
  (* | Switch (_, _, _, _) -> [] *)
  (* | Lambda (_, _, _, _, _) -> [] *)
  | _ -> terr_none ()

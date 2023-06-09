open E_Stmt
open Source
open T_Err

let terr_none () : T_Err.t list = []

let set_terr_stmt (tctx : T_Ctx.t) (tstmt_f : unit -> unit) : T_Err.t list =
  try tstmt_f () |> fun () -> []
  with T_Err.TypeError terr ->
    [ { terr with src = T_Err.stmt_tkn (T_Ctx.get_stmt tctx) } ]

let type_assign (tctx : T_Ctx.t) (var : string) (tvar : E_Type.t option)
    (expr : E_Expr.t) : unit =
  let tdefault = T_Ctx.default_tvar E_Type.AnyType in
  let tprev = Option.default tdefault (T_Ctx.tenv_find tctx var) in
  let rtprev, mtprev = (T_Ctx.get_tvar_rt tprev, T_Ctx.get_tvar_mt tprev) in
  let tref = Option.default rtprev tvar in
  let texpr = T_Expr.safe_type_expr tctx expr tref in
  if mtprev || rtprev = tref then
    T_Ctx.create_tvar tref texpr mtprev |> T_Ctx.tenv_update tctx var
  else T_Err.raise (T_Err.BadTypeUpdate (rtprev, tref))

let type_return (tctx : T_Ctx.t) (expr : E_Expr.t) : unit =
  let tret = Option.default E_Type.AnyType (T_Ctx.get_curr_return_t tctx) in
  try ignore (T_Expr.safe_type_expr tctx expr tret)
  with T_Err.TypeError terr -> (
    match terr.errs with
    | T_Err.BadValue (tref, texpr) :: _ ->
        T_Err.update terr (T_Err.BadReturn (tref, texpr))
    | _ -> failwith "Typed ECMA-SL: T_Stmt.type_return")

let type_guard (tctx : T_Ctx.t) (expr : E_Expr.t) : T_Err.t list =
  try T_Expr.safe_type_expr tctx expr E_Type.BooleanType |> fun _ -> []
  with T_Err.TypeError terr -> (
    match terr.errs with
    | T_Err.BadValue (tref, texpr) :: _ ->
        set_terr_stmt tctx (fun () ->
            T_Err.update terr (T_Err.BadExpectedType (tref, texpr)))
    | _ -> set_terr_stmt tctx (fun () -> T_Err.continue terr))

let apply_constrains (tctx : T_Ctx.t) (form : T_Constraint.t) : T_Err.t list =
  try T_Constraint.apply tctx form |> fun () -> []
  with T_Err.TypeError terr -> (
    match terr.errs with
    | T_Err.NoOverlapComp _ :: _ ->
        set_terr_stmt tctx (fun () -> T_Err.continue terr)
    | _ -> [])

let type_ifelse (tctx : T_Ctx.t) (expr : E_Expr.t) (stmt1 : E_Stmt.t)
    (stmt2 : E_Stmt.t option)
    (type_stmt_f : T_Ctx.t -> E_Stmt.t -> T_Err.t list) : T_Err.t list =
  let terrGuard = type_guard tctx expr in
  let stmt2 = Option.default (Skip @@ no_region) stmt2 in
  let tctx1, tctx2 = (T_Ctx.copy tctx, T_Ctx.copy tctx) in
  let form1 = T_Constraint.generate tctx expr in
  let form2 =
    if stmt2 <> Skip @@ no_region then T_Constraint.Not form1
    else T_Constraint.NoConstraint
  in
  let terrForm1 = apply_constrains tctx1 form1 in
  let terrForm2 = apply_constrains tctx2 form2 in
  let terrsStmt1 = type_stmt_f tctx1 stmt1 in
  let terrsStmt2 = type_stmt_f tctx2 stmt2 in
  let _ = T_Ctx.tenv_intersect tctx [ tctx1; tctx2 ] in
  List.concat [ terrGuard; terrForm1; terrForm2; terrsStmt1; terrsStmt2 ]

let type_while (tctx : T_Ctx.t) (expr : E_Expr.t) (stmt : E_Stmt.t)
    (type_stmt_fun : T_Ctx.t -> E_Stmt.t -> T_Err.t list) : T_Err.t list =
  let terrGuard = type_guard tctx expr in
  let tctx' = T_Ctx.tenv_lock (T_Ctx.copy (T_Ctx.tenv_unnarrow tctx)) in
  let form = T_Constraint.generate tctx' expr in
  let terrForm = apply_constrains tctx' form in
  let terrsStmt = type_stmt_fun tctx' stmt in
  List.concat [ terrGuard; terrForm; terrsStmt ]

let type_fassign (tctx : T_Ctx.t) (oe : E_Expr.t) (fe : E_Expr.t)
    (expr : E_Expr.t) : unit =
  let _rt_of_nt rtoe nt =
    match rtoe with E_Type.SigmaType _ | E_Type.UnionType _ -> nt | _ -> rtoe
  in
  let _type_fassign fn rtoe nt =
    let tref = T_Expr.type_fld_lookup oe fe fn (_rt_of_nt rtoe nt) in
    ignore (T_Expr.safe_type_expr tctx expr tref)
  in
  let rtoe, ntoe = T_Expr.full_type_expr tctx oe in
  match (fe, ntoe) with
  | E_Expr.Val (Val.Str fn), E_Type.UnionType nts ->
      List.iter (_type_fassign fn rtoe) nts
  | E_Expr.Val (Val.Str fn), _ -> List.iter (_type_fassign fn rtoe) [ ntoe ]
  | _ -> ()

let type_match (tctx : T_Ctx.t) (expr : E_Expr.t)
    (pats : (E_Pat.t * E_Stmt.t) list)
    (type_stmt_f : T_Ctx.t -> E_Stmt.t -> T_Err.t list) : T_Err.t list =
  let _update_tvar_f tctx (x, t) =
    T_Ctx.create_tvar t t true |> T_Ctx.tenv_update tctx x
  in
  let _update_tenv tctx patResult =
    match patResult with
    | T_Pattern.Succ patUpdates ->
        List.iter (_update_tvar_f tctx) patUpdates |> fun () -> []
    | T_Pattern.Err (terr, vars) ->
        List.map (fun v -> (v, E_Type.AnyType)) vars
        |> List.iter (_update_tvar_f tctx)
        |> fun () -> [ terr ]
  in
  let _type_match_case_f (stmt, patResult) =
    let tctx' = T_Ctx.copy tctx in
    let terrUpdate = _update_tenv tctx' patResult in
    let terrsStmt = type_stmt_f tctx' stmt in
    List.concat [ terrUpdate; terrsStmt ]
  in
  let _test_complete_model sigmaModel =
    try T_Pattern.test_complete_model sigmaModel |> fun () -> []
    with T_Err.TypeError terr -> [ terr ]
  in
  let _type_match d ts =
    let sigmaModel = T_Pattern.generate_sigma_model ts d in
    let _type_match_pat_f pat = T_Pattern.type_match_pattern sigmaModel d pat in
    let patResults = List.map (fun (p, s) -> (s, _type_match_pat_f p)) pats in
    let terrsComplete = _test_complete_model sigmaModel in
    let terrs = List.map _type_match_case_f patResults in
    List.append (List.concat terrs) terrsComplete
  in
  try
    let rtexpr, ntexpr = T_Expr.full_type_expr tctx expr in
    match (rtexpr, ntexpr) with
    | _, E_Type.AnyType -> []
    | E_Type.SigmaType (d, _), E_Type.UnionType ts -> _type_match d ts
    | E_Type.SigmaType (d, _), _ -> _type_match d [ ntexpr ]
    | _ ->
        set_terr_stmt tctx (fun () ->
            T_Err.raise (T_Err.BadSigma rtexpr) ~tkn:(T_Err.expr_tkn expr))
  with T_Err.TypeError terr ->
    let terr' = { terr with tkn = T_Err.expr_tkn expr } in
    set_terr_stmt tctx (fun () -> T_Err.continue terr')

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
      let _return_f () = type_return tctx e in
      set_terr_stmt tctx _return_f
  (* | Wrapper (_, _) -> [] *)
  | Assign (x, t, e) ->
      let _assign_f () = type_assign tctx x t e in
      set_terr_stmt tctx _assign_f
  (* | GlobAssign (_, _) -> [] *)
  | Block stmts -> List.concat (List.map (fun s -> type_stmt tctx s) stmts)
  | If (e, s1, s2, _, _) -> type_ifelse tctx e s1 s2 type_stmt
  (* | EIf (_, _) -> [] *)
  | While (e, s) -> type_while tctx e s type_stmt
  (* | ForEach (_, _, _, _, _) -> [] *)
  | FieldAssign (oe, fe, e) ->
      let _fassign_f () = type_fassign tctx oe fe e in
      set_terr_stmt tctx _fassign_f
  (* | FieldDelete (_, _) -> [] *)
  | ExprStmt e ->
      let _expr_f () = ignore (T_Expr.safe_type_expr tctx e E_Type.AnyType) in
      set_terr_stmt tctx _expr_f
  (* | RepeatUntil (_, _, _) -> [] *)
  | MatchWith (e, pats) -> type_match tctx e pats type_stmt
  (* | MacroApply (_, _) -> [] *)
  (* | Switch (_, _, _, _) -> [] *)
  (* | Lambda (_, _, _, _, _) -> [] *)
  | _ -> terr_none ()

open EslSyntax

let rec type_stmt (s : EStmt.t) (tctx : TCtx.t) : TCtx.t =
  TCtx.safe_exec (type_stmt' s) tctx

and type_stmt' (s : EStmt.t) (tctx : TCtx.t) : TCtx.t =
  match s.it with
  | Skip -> tctx
  | Debug s' -> type_stmt s' tctx
  | Block ss -> type_block ss tctx
  | ExprStmt e -> type_expr_stmt e tctx
  | Print e -> type_print e tctx
  | Return e -> type_return e tctx
  | Assign (x, t, e) -> type_assign x t e tctx
  | GAssign _ -> tctx
  | FieldAssign _ -> tctx
  | FieldDelete _ -> tctx
  | If _ -> tctx
  | While _ -> tctx
  | ForEach _ -> tctx
  | RepeatUntil _ -> tctx
  | Switch _ -> tctx
  | MatchWith _ -> tctx
  | Lambda _ -> tctx
  | MacroApply _ -> tctx
  | Throw _ -> tctx
  | Fail _ -> tctx
  | Assert _ -> tctx
  | Wrapper _ -> tctx

and type_block (ss : EStmt.t list) (tctx : TCtx.t) : TCtx.t =
  List.fold_left (fun tctx s -> type_stmt s tctx) tctx ss

and type_expr_stmt (e : EExpr.t) (tctx : TCtx.t) : TCtx.t =
  TExpr.type_expr tctx e |> fun _ -> tctx

and type_print (e : EExpr.t) (tctx : TCtx.t) : TCtx.t =
  TExpr.type_expr tctx e |> fun _ -> tctx

and type_return (e : EExpr.t) (tctx : TCtx.t) : TCtx.t =
  let tref = TCtx.curr_treturn tctx in
  let tsrc = TExpr.type_expr tctx e in
  try TSubtyping.type_check tref tsrc |> fun () -> tctx
  with Typing_error.Error err ->
    Typing_error.(update (BadReturn (tref, tsrc)) err |> raise)

and type_assign (x : Id.t) (t : EType.t option) (e : EExpr.t) (tctx : TCtx.t) :
  TCtx.t =
  let update_tenv tref = TCtx.(tvar_create t tref |> tenv_set tctx x) in
  let typecheck tref tsrc =
    try TSubtyping.type_check tref tsrc
    with Typing_error.Error err ->
      update_tenv tref |> fun () -> Typing_error.(raise err)
  in
  let tref = EType.resolve_topt t in
  let tsrc = TExpr.type_expr tctx e in
  typecheck tref tsrc;
  update_tenv tsrc;
  tctx

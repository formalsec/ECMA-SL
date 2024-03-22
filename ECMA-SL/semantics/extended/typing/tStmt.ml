open EslSyntax

let rec type_stmt (s : EStmt.t) (tctx : TCtx.t) : TCtx.t =
  TCtx.safe_exec (type_stmt' s) tctx

and type_stmt' (s : EStmt.t) (tctx : TCtx.t) : TCtx.t =
  match s.it with
  | Skip -> tctx
  | Debug s' -> type_stmt s' tctx
  | Block ss -> type_block ss tctx
  | Print e -> type_print e tctx
  | Return e -> type_return e tctx
  | ExprStmt e -> type_expr_stmt e tctx
  | Assign _ -> tctx
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

and type_print (e : EExpr.t) (tctx : TCtx.t) : TCtx.t =
  TExpr.type_expr e |> fun _ -> tctx

and type_return (e : EExpr.t) (tctx : TCtx.t) : TCtx.t =
  let tret = TCtx.curr_treturn tctx in
  let tsrc = TExpr.type_expr e in
  try TSubtyping.type_check tret tsrc |> fun () -> tctx
  with Typing_error.Error err ->
    Typing_error.(update (BadReturn (tret, tsrc)) err |> raise)

and type_expr_stmt (e : EExpr.t) (tctx : TCtx.t) : TCtx.t =
  TExpr.type_expr e |> fun _ -> tctx

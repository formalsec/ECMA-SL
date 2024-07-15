open EslSyntax
open EslSyntax.Source

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
  | Assign (x, tx, e) -> type_assign x tx e tctx
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

and type_assign (x : Id.t) (tx : EType.t option) (e : EExpr.t) (tctx : TCtx.t) :
  TCtx.t =
  let update_tenv tref = TCtx.(tvar_create tx tref |> tenv_set tctx x) in
  let tdefault = TCtx.tvar_create None (EType.AnyType @?> none) in
  let tprev = Option.value ~default:tdefault (TCtx.tenv_find tctx x) in
  let tref = Option.value ~default:(tprev.tref @?> x.at) tx in
  let tsrc = TExpr.type_expr tctx e in
  update_tenv tref;
  TSubtyping.type_check tref tsrc |> fun () -> tctx

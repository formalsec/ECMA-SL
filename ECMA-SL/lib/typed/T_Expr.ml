open E_Expr

let type_expr (expr : E_Expr.t) : E_Type.t option =
  match expr with
  | Val v -> None
  | Var x -> None
  | GVar _ -> None
  | Const c -> None
  | UnOpt (_, _) -> None
  | BinOpt (_, _, _) -> None
  | EBinOpt (_, _, _) -> None
  | TriOpt (_, _, _, _) -> None
  | NOpt (_, _) -> None
  | Call (_, _, _) -> None
  | ECall (_, _) -> None
  | NewObj _ -> None
  | Lookup (_, _) -> None
  | Curry (_, _) -> None
  | Symbolic (_, _) -> None

open E_Expr

let type_expr (expr : E_Expr.t) : E_Type.t option =
  match expr with
  | Val v -> failwith "[Type Checker] TODO: val"
  | Var x -> failwith "[Type Checker] TODO: var"
  | GVar _ -> failwith "[Type Checker] TODO: gvar"
  | Const c -> failwith "[Type Checker] TODO: const"
  | UnOpt (_, _) -> failwith "[Type Checker] TODO: unopt"
  | BinOpt (_, _, _) -> failwith "[Type Checker] TODO: binopt"
  | EBinOpt (_, _, _) -> failwith "[Type Checker] TODO: ebinopt"
  | TriOpt (_, _, _, _) -> failwith "[Type Checker] TODO: triopt"
  | NOpt (_, _) -> failwith "[Type Checker] TODO: nopt"
  | Call (_, _, _) -> failwith "[Type Checker] TODO: call"
  | ECall (_, _) -> failwith "[Type Checker] TODO: ecall"
  | NewObj _ -> failwith "[Type Checker] TODO: newobj"
  | Lookup (_, _) -> failwith "[Type Checker] TODO: lookup"
  | Curry (_, _) -> failwith "[Type Checker] TODO: curry"
  | Symbolic (_, _) -> failwith "[Type Checker] TODO: symbolic"

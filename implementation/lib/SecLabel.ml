type t =
  | AsgnLab of (string * Expr.t)
  | EmptyLab
  | BranchLab of (Expr.t * Stmt.t)
  | MergeLab
  | CallLab of ((Expr.t list)* string * string)
  | RetLab of Expr.t

let str (label :t) : string =
  match label with
  |EmptyLab -> "EmptyLab"
  |MergeLab -> "MergeLab"
  |RetLab e-> "RetLab ("^ (Expr.str e)^ ")"
  |AsgnLab (st,exp) -> "AsgnLab ("^ (Expr.str exp) ^", "^st ^")"
  |BranchLab (exp, stmt) -> "BranchLab (" ^(Expr.str exp) ^"),{ "^(Stmt.str stmt)^"}"
  |CallLab (exp,x,f)-> "CallLab ("^(String.concat "; " (List.map Expr.str exp))^", "^ x^")"

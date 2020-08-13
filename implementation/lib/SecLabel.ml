type t =
  | AsgnLab of (string * Expr.t)
  | EmptyLab
  | BranchLab of (Expr.t * Stmt.t)
  | MergeLab
  | CallLab of ((Expr.t list)* string * string)
  | RetLab of Expr.t
  | UpgradeVarLab of (string * SecLevel.t)
  | UpgradePropVal
  | UpgradePropLevel
 

let str (label :t) : string =
  match label with
  |EmptyLab -> "EmptyLab"
  |MergeLab -> "MergeLab"
  |RetLab e-> "RetLab ("^ (Expr.str e)^ ")"
  |AsgnLab (st,exp) -> "AsgnLab ("^ (Expr.str exp) ^", "^st ^")"
  |BranchLab (exp, stmt) -> "BranchLab (" ^(Expr.str exp) ^"),{ "^(Stmt.str stmt)^"}"
  |CallLab (exp,x,f)-> "CallLab ("^(String.concat "; " (List.map Expr.str exp))^", "^ x^")"
  
let intercept (func:string) (vs:Val.t list): t option =
   match (func,vs) with
   | ("upgVar",[Val.Str x; Val.Str lev_str]) ->  UpgradeVarLab(x,SecLevel.parse_lvl lev_str)
   
   (*Ver tese andre para checkar todas a labels*)

type t =
  | AsgnLab of (string * Expr.t)
  | EmptyLab
  | BranchLab of (Expr.t * Stmt.t)
  | MergeLab
  | CallLab of ((Expr.t list)* string * string)
  | RetLab of Expr.t
  | UpgVarLab of (string * SecLevel.t)
  | UpgPropExistsLab of (Loc.t * string * SecLevel.t)
  | UpgPropValLab of (Loc.t * string * SecLevel.t)
  | UpgStructLab of (Loc.t * SecLevel.t)



let str (label :t) : string =
  match label with
  |EmptyLab -> "EmptyLab"
  |MergeLab -> "MergeLab"
  |RetLab e-> "RetLab ("^ (Expr.str e)^ ")"
  |AsgnLab (st,exp) -> "AsgnLab ("^ (Expr.str exp) ^", "^st ^")"
  |BranchLab (exp, stmt) -> "BranchLab (" ^(Expr.str exp) ^"),{ "^(Stmt.str stmt)^"}"
  |CallLab (exp,x,f) -> "CallLab ("^(String.concat "; " (List.map Expr.str exp))^", "^ x^")"
  |UpgVarLab (x, lvl) -> "UpgVarLab"
  |UpgPropValLab (loc, x, lvl) -> "UpgPropLab"
  |UpgStructLab (loc, lvl) -> "UpgStructLab"


let interceptor (func:string) (vs:Val.t list): t option =
   match (func,vs) with
   | ("upgVar",[Val.Str x; Val.Str lev_str]) ->  Some (UpgVarLab (x,SecLevel.parse_lvl lev_str))
   | ("upgPropExists",[Val.Loc loc; Val.Str x; Val.Str lev_str])-> Some (UpgPropExistsLab (loc,x,SecLevel.parse_lvl lev_str))
   | ("upgStruct",[Val.Loc loc; Val.Str lev_str])-> Some (UpgStructLab (loc, SecLevel.parse_lvl lev_str))
   | ("upgPropVal", [Val.Loc loc; Val.Str x; Val.Str lev_str])-> Some (UpgPropValLab (loc,x,SecLevel.parse_lvl lev_str))
   |_ -> None

   (*Ver tese andre para checkar todas a labels*)

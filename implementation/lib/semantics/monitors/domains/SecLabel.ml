type t =
  | AssignLab of (string * Expr.t)
  | EmptyLab
  | BranchLab of (Expr.t * Stmt.t)
  | MergeLab
  | AssignCallLab of ((Expr.t list)* string * string)
  | ReturnLab of Expr.t
  | FieldAssignLab of (Loc.t * Field.t * Expr.t * Expr.t * Expr.t)
  | FieldLookupLab of (string * Loc.t * Field.t * Expr.t * Expr.t)
  | FieldDeleteLab of (Loc.t * Field.t * Expr.t * Expr.t)
  (* Direct Security Level Upgrades *)
  | UpgVarLab of (string * SecLevel.t)
  | UpgPropExistsLab of (Loc.t * string * SecLevel.t)
  | UpgPropValLab of (Loc.t * string * SecLevel.t)
  | UpgStructExistsLab of (Loc.t * SecLevel.t)
  | UpgStructValLab of (Loc.t * SecLevel.t)



let str (label :t) : string =
  match label with
  | EmptyLab ->
    "EmptyLab"
  | MergeLab ->
    "MergeLab"
  | ReturnLab e ->
    "RetLab ("^ (Expr.str e)^ ")"
  | AssignLab (st,exp) ->
    "AsgnLab ("^ (Expr.str exp) ^", "^st ^")"
  | BranchLab (exp, stmt) ->
    "BranchLab (" ^(Expr.str exp) ^"),{ "^(Stmt.str stmt)^"}"
  | AssignCallLab (exp,x,f) ->
    "AssignCallLab ("^(String.concat "; " (List.map Expr.str exp))^", "^ x^")"
  | UpgVarLab (x, lvl) ->
    "UpgVarLab"
  | UpgPropValLab (loc, x, lvl) ->
    "UpgPropLab"
  | UpgStructValLab (loc, lvl) ->
    "UpgStructLab"
  | UpgStructExistsLab (loc, lvl) ->
    "UpgStructLab"
  | _ ->
    "Missing str"



let interceptor (func:string) (vs:Val.t list): t option =
  match (func,vs) with
  | ("upgVar",[Val.Str x; Val.Str lev_str]) ->  Some (UpgVarLab (x,SecLevel.parse_lvl lev_str))
  | ("upgPropExists",[Val.Loc loc; Val.Str x; Val.Str lev_str])-> Some (UpgPropExistsLab (loc,x,SecLevel.parse_lvl lev_str))
  | ("upgPropVal", [Val.Loc loc; Val.Str x; Val.Str lev_str])-> Some (UpgPropValLab (loc,x,SecLevel.parse_lvl lev_str))
  | ("upgStructExists",[Val.Loc loc; Val.Str lev_str])-> Some (UpgStructExistsLab (loc, SecLevel.parse_lvl lev_str))
  | ("upgStructVal",[Val.Loc loc; Val.Str lev_str])-> Some (UpgStructValLab (loc, SecLevel.parse_lvl lev_str))
  | _ -> None

(*Ver tese andre para checkar todas a labels*)

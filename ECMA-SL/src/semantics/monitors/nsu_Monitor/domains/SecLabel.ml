open Val

exception Except of string

type 'sl t =
  | AssignLab of (string * Expr.t)
  | EmptyLab
  | PrintLab of Expr.t
  | BranchLab of (Expr.t * Stmt.t)
  | MergeLab
  | AssignCallLab of (string list * Expr.t list * string * string)
  | ReturnLab of Expr.t
  | FieldAssignLab of (Loc.t * Field.t * Expr.t * Expr.t * Expr.t)
  | FieldLookupLab of (string * Loc.t * Field.t * Expr.t * Expr.t)
  | FieldDeleteLab of (Loc.t * Field.t * Expr.t * Expr.t)
  | AssignInObjCheckLab of (string * Field.t * Loc.t * Expr.t * Expr.t)
  | NewLab of (string * Loc.t)
  (* Direct Security Level Upgrades *)
  | UpgVarLab of (string * 'sl)
  | UpgPropExistsLab of (Loc.t * string * Expr.t * Expr.t * 'sl)
  | UpgPropValLab of (Loc.t * string * Expr.t * Expr.t * 'sl)
  | UpgStructLab of (Loc.t * Expr.t * 'sl)
  | UpgObjectLab of (Loc.t * Expr.t * 'sl)
  (**)
  | SetTopLab of string
  | AllowFlowLab of string * string

let str (_sl_str : 'sl -> string) (label : 'sl t) : string =
  match label with
  | EmptyLab -> "EmptyLab"
  | PrintLab e -> "PrintLab (" ^ Expr.str e ^ ")"
  | MergeLab -> "MergeLab"
  | ReturnLab e -> "RetLab (" ^ Expr.str e ^ ")"
  | AssignLab (st, exp) -> "AsgnLab (" ^ Expr.str exp ^ ", " ^ st ^ ")"
  | BranchLab (exp, stmt) ->
      "BranchLab (" ^ Expr.str exp ^ "),{ " ^ Stmt.str stmt ^ "}"
  | AssignCallLab (_params, exp, x, _f) ->
      "AssignCallLab ("
      ^ String.concat "; " (List.map Expr.str exp)
      ^ ", " ^ x ^ ")"
  | AssignInObjCheckLab _ -> "AssingnInObjCheckLab"
  | UpgVarLab _ -> "UpgVarLab"
  | UpgPropValLab _ -> "UpgPropLab"
  | UpgPropExistsLab _ -> "UpgPropLab"
  | UpgObjectLab _ -> "UpgStructLab"
  | UpgStructLab _ -> "UpgStructLab"
  | SetTopLab st -> "TopLevelLab ( " ^ st ^ " )"
  | AllowFlowLab (st1, st2) -> "AllowFlowLab ( " ^ st1 ^ ",  " ^ st2 ^ " )"
  | _ -> "Missing str"

let interceptor (parse_sl : string -> 'sl) (func : string) (vs : Val.t list)
    (es : Expr.t list) : 'sl t option =
  match (func, vs, es) with
  | ( "upgVar",
      [ Val.Str x; Val.Str lev_str ],
      [ Expr.Val (Str x'); Expr.Val (Str lev_str') ] )
    when x = x' && lev_str = lev_str' ->
      Some (UpgVarLab (x, parse_sl lev_str))
  | ( "upgPropExists",
      [ Val.Loc loc; Val.Str x; Val.Str lev_str ],
      [ e_o; e_f; Expr.Val (Str lev_str') ] )
    when lev_str = lev_str' ->
      Some (UpgPropExistsLab (loc, x, e_o, e_f, parse_sl lev_str))
  | ( "upgPropExists",
      [ Val.Loc _loc; Val.Str _x; Val.Str _lev_str ],
      [ _; _; _ ] ) ->
      raise (Except "Level is not a literal ") (*Gerar uma exception*)
  | ( "upgPropVal",
      [ Val.Loc loc; Val.Str x; Val.Str lev_str ],
      [ e_o; e_f; Expr.Val (Str lev_str') ] )
    when lev_str = lev_str' ->
      Some (UpgPropValLab (loc, x, e_o, e_f, parse_sl lev_str))
  | "upgPropVal", [ Val.Loc _; Val.Str _x; Val.Str _lev_str ], [ _; _; _ ]
    ->
      raise (Except "Level is not a literal ")
  | ( "upgStruct",
      [ Val.Loc loc; Val.Str lev_str ],
      [ e_o; Expr.Val (Str lev_str') ] )
    when lev_str = lev_str' ->
      Some (UpgStructLab (loc, e_o, parse_sl lev_str))
  | "upgStruct", [ Val.Loc _loc; Val.Str _lev_str ], [ _e_o; _ ] ->
      raise (Except "Level is not a literal ")
  | ( "upgObject",
      [ Val.Loc loc; Val.Str lev_str ],
      [ e_o; Expr.Val (Str lev_str') ] )
    when lev_str = lev_str' ->
      Some (UpgObjectLab (loc, e_o, parse_sl lev_str))
  | "upgObject", [ Val.Loc _loc; Val.Str _lev_str ], [ _e_o; _ ] ->
      raise (Except "Level is not a literal ")
  | "setTop", [ Val.Str str ], [ Expr.Val (Str str') ] when str = str' ->
      Some (SetTopLab str)
  | "setTop", [ Val.Str _str ], [ _ ] -> raise (Except "Level is not a string ")
  | ( "allowFlow",
      [ Val.Str str1; Val.Str str2 ],
      [ Expr.Val (Str str1'); Expr.Val (Str str2') ] )
    when str1 = str1' && str2 = str2' ->
      Some (AllowFlowLab (str1, str2))
  | "allowFlow", [ _; _ ], [ _; _ ] -> raise (Except "Level is not a string ")
  | _ -> None

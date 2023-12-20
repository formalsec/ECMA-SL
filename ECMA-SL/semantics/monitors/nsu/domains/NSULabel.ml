open Val

type 'sl t =
  | EmptyLab
  | MergeLab
  | PrintLab of Expr.t
  | ReturnLab of Expr.t
  | AssignLab of (string * Expr.t)
  | AssignCallLab of (string * string * string list * Expr.t list)
  | AssignInObjCheckLab of (string * string * Loc.t * Expr.t * Expr.t)
  | NewLab of (string * Loc.t)
  | FieldAssignLab of (Loc.t * string * Expr.t * Expr.t * Expr.t)
  | FieldLookupLab of (string * Loc.t * string * Expr.t * Expr.t)
  | FieldDeleteLab of (Loc.t * string * Expr.t * Expr.t)
  | BranchLab of (Expr.t * Stmt.t)
  (* Direct Security Level Upgrades *)
  | UpgVarLab of (string * 'sl)
  | UpgPropValLab of (Loc.t * string * Expr.t * Expr.t * 'sl)
  | UpgPropExistsLab of (Loc.t * string * Expr.t * Expr.t * 'sl)
  | UpgObjectLab of (Loc.t * Expr.t * 'sl)
  | UpgStructLab of (Loc.t * Expr.t * 'sl)
  (**)
  | SetTopLab of string
  | AllowFlowLab of string * string

let str (label : 'sl t) (_sl_printer : 'sl -> string) : string =
  match label with
  | EmptyLab -> "EmptyLab"
  | MergeLab -> "MergeLab"
  | PrintLab e -> Printf.sprintf "PrintLab (%s)" (Expr.str e)
  | ReturnLab e -> Printf.sprintf "RetLab (%s)" (Expr.str e)
  | AssignLab (x, e) -> Printf.sprintf "AssignLab (%s, %s)" x (Expr.str e)
  | AssignCallLab (x, _f, _params, es) ->
    Printf.sprintf "AssignCallLab(%s, [%s])" x
      (List.map Expr.str es |> String.concat ";")
  | AssignInObjCheckLab _ -> "AssignInObjCheckLab"
  | NewLab _ -> "FIXME: NewLab.str"
  | FieldAssignLab _ -> "FIXME: FieldAssignLab.str"
  | FieldLookupLab _ -> "FIXME: FieldLookupLab.str"
  | FieldDeleteLab _ -> "FIXME: FieldDeleteLab.str"
  | BranchLab (expr, stmt) ->
    Printf.sprintf "BranchLab (%s){%s}" (Expr.str expr) (Stmt.str stmt)
  | UpgVarLab _ -> "UpgVarLab"
  | UpgPropValLab _ -> "UpgPropLab"
  | UpgPropExistsLab _ -> "UpgPropLab"
  | UpgObjectLab _ -> "UpgStructLab"
  | UpgStructLab _ -> "UpgStructLab"
  | SetTopLab st -> Printf.sprintf "TopLevelLab (%s)" st
  | AllowFlowLab (st1, st2) -> Printf.sprintf "AllowFlowLab (%s, %s)" st1 st2

let interceptor (parse_sl : string -> 'sl) (func : string) (vs : Val.t list)
  (es : Expr.t list) : 'sl t option =
  match (func, vs, es) with
  | ( "upgVar"
    , [ Val.Str x; Val.Str lvl_str ]
    , [ Expr.Val (Str x'); Expr.Val (Str lvl_str') ] )
    when x = x' && lvl_str = lvl_str' ->
    Some (UpgVarLab (x, parse_sl lvl_str))
  | ( "upgPropExists"
    , [ Val.Loc loc; Val.Str x; Val.Str lvl_str ]
    , [ e_o; e_f; Expr.Val (Str lvl_str') ] )
    when lvl_str = lvl_str' ->
    Some (UpgPropExistsLab (loc, x, e_o, e_f, parse_sl lvl_str))
  | ( "upgPropExists"
    , [ Val.Loc _loc; Val.Str _x; Val.Str _lvl_str ]
    , [ _; _; _ ] ) ->
    raise (NSUException.Except "Level is not a literal")
  | ( "upgPropVal"
    , [ Val.Loc loc; Val.Str x; Val.Str lvl_str ]
    , [ e_o; e_f; Expr.Val (Str lvl_str') ] )
    when lvl_str = lvl_str' ->
    Some (UpgPropValLab (loc, x, e_o, e_f, parse_sl lvl_str))
  | ("upgPropVal", [ Val.Loc _; Val.Str _x; Val.Str _lvl_str ], [ _; _; _ ]) ->
    raise (NSUException.Except "Level is not a literal")
  | ( "upgStruct"
    , [ Val.Loc loc; Val.Str lvl_str ]
    , [ e_o; Expr.Val (Str lvl_str') ] )
    when lvl_str = lvl_str' ->
    Some (UpgStructLab (loc, e_o, parse_sl lvl_str))
  | ("upgStruct", [ Val.Loc _loc; Val.Str _lvl_str ], [ _e_o; _ ]) ->
    raise (NSUException.Except "Level is not a literal")
  | ( "upgObject"
    , [ Val.Loc loc; Val.Str lvl_str ]
    , [ e_o; Expr.Val (Str lvl_str') ] )
    when lvl_str = lvl_str' ->
    Some (UpgObjectLab (loc, e_o, parse_sl lvl_str))
  | ("upgObject", [ Val.Loc _loc; Val.Str _lvl_str ], [ _e_o; _ ]) ->
    raise (NSUException.Except "Level is not a literal ")
  | ("setTop", [ Val.Str str ], [ Expr.Val (Str str') ]) when str = str' ->
    Some (SetTopLab str)
  | ("setTop", [ Val.Str _str ], [ _ ]) ->
    raise (NSUException.Except "Level is not a string")
  | ( "allowFlow"
    , [ Val.Str str1; Val.Str str2 ]
    , [ Expr.Val (Str str1'); Expr.Val (Str str2') ] )
    when str1 = str1' && str2 = str2' ->
    Some (AllowFlowLab (str1, str2))
  | ("allowFlow", [ _; _ ], [ _; _ ]) ->
    raise (NSUException.Except "Level is not a string")
  | _ -> None

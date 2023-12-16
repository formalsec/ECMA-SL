type t =
  | NoConstraint
  | Expr of E_Expr.t
  | Not of t
  | And of t * t
  | Or of t * t

let rec is_container (e : E_Expr.t) : bool =
  match e with
  | E_Expr.Var _ -> true
  | E_Expr.Lookup _ -> true
  | E_Expr.UnOpt (Operators.Typeof, e') -> is_container e'
  | _ -> false

let rec generate (tctx : T_Ctx.t) (expr : E_Expr.t) : t =
  match expr with
  | E_Expr.UnOpt (Operators.LogicalNot, e) -> Not (generate tctx e)
  | E_Expr.BinOpt (Operators.LogicalAnd, e1, e2) ->
    let ce1 = generate tctx e1 in
    let ce2 = generate tctx e2 in
    And (ce1, ce2)
  | E_Expr.BinOpt (Operators.LogicalOr, e1, e2) ->
    let ce1 = generate tctx e1 in
    let ce2 = generate tctx e2 in
    Or (ce1, ce2)
  | E_Expr.BinOpt (Operators.Eq, e1, e2) -> (
    let revExpr = Expr (E_Expr.BinOpt (Operators.Eq, e2, e1)) in
    match (is_container e1, is_container e2) with
    | (true, true) -> And (Expr expr, revExpr)
    | _ -> Expr expr )
  | _ -> NoConstraint

let rec str (f : t) : string =
  match f with
  | NoConstraint -> "none"
  | Expr e -> E_Expr.str e
  | Not f' -> "!(" ^ str f' ^ ")"
  | And (f1, f2) -> "(" ^ str f1 ^ " && " ^ str f2 ^ ")"
  | Or (f1, f2) -> "(" ^ str f1 ^ " || " ^ str f2 ^ ")"

let rec nnf_converter (f : t) : t =
  let _nnf_negate f1 f2 = (nnf_converter (Not f1), nnf_converter (Not f2)) in
  let _nnf_propagate f1 f2 = (nnf_converter f1, nnf_converter f2) in
  match f with
  | Not NoConstraint -> NoConstraint
  | Not (And (f1, f2)) -> _nnf_negate f1 f2 |> fun (f1', f2') -> Or (f1', f2')
  | Not (Or (f1, f2)) -> _nnf_negate f1 f2 |> fun (f1', f2') -> And (f1', f2')
  | Not (Not f') -> f'
  | And (f1, f2) -> _nnf_propagate f1 f2 |> fun (f1', f2') -> And (f1', f2')
  | Or (f1, f2) -> _nnf_propagate f1 f2 |> fun (f1', f2') -> Or (f1', f2')
  | _ -> f

let rec nnf_cleaner (f : t) : t =
  let _nnf_cleaner f1 f2 = (nnf_cleaner f1, nnf_cleaner f2) in
  match f with
  | And (f1, f2) -> (
    match _nnf_cleaner f1 f2 with
    | (NoConstraint, NoConstraint) -> NoConstraint
    | (NoConstraint, f2') -> f2'
    | (f1', NoConstraint) -> f1'
    | (f1', f2') -> And (f1', f2') )
  | Or (f1, f2) -> (
    match _nnf_cleaner f1 f2 with
    | (NoConstraint, NoConstraint) -> NoConstraint
    | (NoConstraint, f2') -> f2'
    | (f1', NoConstraint) -> f1'
    | (f1', f2') -> Or (f1', f2') )
  | _ -> f

let rec nnf_distribute (f : t) : t =
  match f with
  | And (f1, f2) -> And (nnf_distribute f1, nnf_distribute f2)
  | Or (f1, f2) -> (
    let f1' = nnf_distribute f1 in
    let f2' = nnf_distribute f2 in
    match (f1', f2') with
    | (And (f11, f12), _) ->
      nnf_distribute (Or (f11, f2')) |> fun arg1 ->
      nnf_distribute (Or (f12, f2')) |> fun arg2 ->
      nnf_distribute (And (arg1, arg2))
    | (_, And (f21, f22)) ->
      nnf_distribute (Or (f1', f21)) |> fun arg1 ->
      nnf_distribute (Or (f1', f22)) |> fun arg2 ->
      nnf_distribute (And (arg1, arg2))
    | _ -> Or (f1', f2') )
  | _ -> f

let rec cnf_and_clauses (f : t) : t list =
  match f with
  | And (f1, f2) -> List.append (cnf_and_clauses f1) (cnf_and_clauses f2)
  | _ -> [ f ]

let rec cnf_or_clauses (f : t) : t list =
  match f with
  | Or (f1, f2) -> List.append (cnf_or_clauses f1) (cnf_or_clauses f2)
  | _ -> [ f ]

let cnf_converter (f : t) : t list =
  f |> nnf_converter |> nnf_cleaner |> nnf_distribute |> cnf_and_clauses

type element_t = E_Expr.t * constraint_t

and constraint_t =
  { expr : E_Expr.t
  ; tcstr : E_Type.t
  ; isNeq : bool
  ; isTypeof : bool
  }

let create_constraint (expr : E_Expr.t) (tcstr : E_Type.t) (isNeq : bool)
  (isTypeof : bool) : constraint_t =
  { expr; tcstr; isTypeof; isNeq }

let inspect_element (tctx : T_Ctx.t) (expr : E_Expr.t) (isNeq : bool) :
  element_t =
  let choose_container e1 e2 =
    match (is_container e1, is_container e2) with
    | (false, true) -> (e2, e1)
    | _ -> (e1, e2)
  in
  let _eval_type expr =
    match expr with
    | E_Expr.UnOpt (Operators.Typeof, e) ->
      E_Type.to_runtime (T_Expr.type_expr tctx e)
    | _ -> T_Expr.type_expr tctx expr
  in
  match expr with
  | E_Expr.BinOpt (Operators.Eq, e1, e2) -> (
    let (tar, cstr) = choose_container e1 e2 in
    let tcstr = _eval_type cstr in
    match tar with
    | E_Expr.Var _ -> (tar, create_constraint expr tcstr isNeq false)
    | E_Expr.Lookup _ -> (tar, create_constraint expr tcstr isNeq false)
    | E_Expr.UnOpt (Operators.Typeof, tar') ->
      (tar', create_constraint expr tcstr isNeq true)
    | _ -> (tar, create_constraint expr tcstr isNeq false) )
  | _ -> failwith "Typed ECMA-SL: T_Constraint.inspect_element"

let eval_constraint (ttar : E_Type.t) (cstr : constraint_t) : E_Type.t list =
  let _terr ttar cstr =
    let tkn = T_Err.expr_tkn cstr.expr in
    T_Err.raise (T_Err.NoOverlapComp (ttar, cstr.tcstr)) ~tkn
  in
  let _runtime_neq_f b = if cstr.isNeq then not b else b in
  let rec _runtime_cmp_f t =
    match t with
    | E_Type.RuntimeType Type.TypeType -> true
    | E_Type.RuntimeType _ -> t = cstr.tcstr
    | _ -> _runtime_cmp_f (E_Type.to_runtime t)
  in
  if cstr.isTypeof then
    List.filter
      (fun t -> t |> _runtime_cmp_f |> _runtime_neq_f)
      (E_Type.tlst ttar)
  else if cstr.isNeq then
    let tlit = E_Type.unfold_type false ttar in
    let tunfolded = E_Type.unfold_type true ttar in
    let tremoved =
      List.filter (fun t -> List.mem t tlit) (E_Type.tlst cstr.tcstr)
    in
    let tkeep = List.filter (fun t -> not (List.mem t tremoved)) tunfolded in
    E_Type.fold_type tkeep
  else
    let isTypeable_f = T_Typing.is_typeable ttar in
    let tconstraint = List.filter isTypeable_f (E_Type.tlst cstr.tcstr) in
    if tconstraint = [] then _terr ttar cstr else tconstraint

let rec apply_constrain (tctx : T_Ctx.t) (target : E_Expr.t list)
  (tconstraint : E_Type.t) : unit =
  let _test_union_case oe fe fn nt =
    let tref = T_Expr.type_fld_lookup oe fe fn nt in
    List.mem tref (E_Type.tlst tconstraint)
  in
  let rec _filter_union_case oe fexpr ntoe =
    match (fexpr, ntoe) with
    | (_, E_Type.UserDefinedType _) ->
      _filter_union_case oe fexpr (T_Typing.resolve_typedef ntoe)
    | (E_Expr.Val (Val.Str fn), E_Type.UnionType nts)
    | (E_Expr.Val (Val.Str fn), E_Type.SigmaType (_, nts)) ->
      List.filter (_test_union_case oe fexpr fn) nts |> fun ts ->
      T_Narrowing.narrow_type (E_Type.UnionType ts) |> fun t ->
      apply_constrain tctx [ oe ] t
    | _ -> ()
  in
  match target with
  | E_Expr.Var var :: [] -> T_Ctx.tenv_constrain tctx var tconstraint
  | E_Expr.Lookup (oe, fexpr) :: [] ->
    let ntoe = T_Expr.type_expr tctx oe in
    _filter_union_case oe fexpr ntoe
  | _ -> ()

let apply_clause (tctx : T_Ctx.t) (clause : t) : unit =
  let _inspect_element_f element =
    match element with
    | Expr expr -> inspect_element tctx expr false
    | Not (Expr expr) -> inspect_element tctx expr true
    | _ -> failwith "Typed ECMA-SL: T_Constraint.apply_clause"
  in
  let _eval_constraint_f (tar, cstr) =
    eval_constraint (T_Expr.type_expr tctx tar) cstr
  in
  let _get_targets elements =
    let _unique_f e r = if List.mem e r then r else e :: r in
    List.fold_right _unique_f (fst (List.split elements)) []
  in
  if clause <> NoConstraint then
    let elements = List.map _inspect_element_f (cnf_or_clauses clause) in
    let tevals = List.concat (List.map _eval_constraint_f elements) in
    let tconstraint = T_Narrowing.narrow_type (E_Type.UnionType tevals) in
    let targets = _get_targets elements in
    apply_constrain tctx targets tconstraint

let apply (tctx : T_Ctx.t) (f : t) : unit =
  let clauses = cnf_converter f in
  List.iter (apply_clause tctx) clauses

type t =
  | Val of Val.t
  | Var of string
  | UnOpt of (Operator.unopt * t)
  | BinOpt of (Operator.binopt * t * t)
  | TriOpt of (Operator.triopt * t * t * t)
  | NOpt of Operator.nopt * t list
  | Curry of t * t list
  | Symbolic of Type.t * t

module Builder = struct
  let not_expr e = UnOpt (Operator.LogicalNot, e) [@@inline]
  let and_expr e1 e2 = BinOpt (Operator.LogicalAnd, e1, e2) [@@inline]
  let or_expr e1 e2 = BinOpt (Operator.LogicalOr, e1, e2) [@@inline]
end

let rec equal (e1 : t) (e2 : t) : bool =
  match (e1, e2) with
  | (Val v', Val v'') -> Val.equal v' v''
  | (Var x', Var x'') -> x' = x''
  | (UnOpt (op', e'), UnOpt (op'', e'')) -> op' = op'' && equal e' e''
  | (BinOpt (op', e1', e2'), BinOpt (op'', e1'', e2'')) ->
    op' = op'' && equal e1' e1'' && equal e2' e2''
  | (TriOpt (op', e1', e2', e3'), TriOpt (op'', e1'', e2'', e3'')) ->
    op' = op'' && equal e1' e1'' && equal e2' e2'' && equal e3' e3''
  | (NOpt (op', es'), NOpt (op'', es'')) ->
    op' = op'' && List.equal equal es' es''
  | (Curry (x', es'), Curry (x'', es'')) ->
    equal x' x'' && List.equal equal es' es''
  | (Symbolic (t', e'), Symbolic (t'', e'')) ->
    Type.equal t' t'' && equal e' e''
  | _ -> false

let rec copy (e : t) : t =
  match e with
  | Val v -> Val (Val.copy v)
  | Var x -> Var x
  | UnOpt (op, e) -> UnOpt (op, copy e)
  | BinOpt (op, e1, e2) -> BinOpt (op, copy e1, copy e2)
  | TriOpt (op, e1, e2, e3) -> TriOpt (op, copy e1, copy e2, copy e3)
  | NOpt (op, es) -> NOpt (op, List.map copy es)
  | Curry (x, es) -> Curry (x, List.map copy es)
  | Symbolic (t, e) -> Symbolic (t, copy e)

let is_val (e : t) : bool =
  match e with
  | Val _ -> true
  | _ -> false

let is_loc (e : t) : bool =
  match e with
  | Val (Val.Loc _) -> true
  | _ -> false

let rec is_symbolic (e : t) : bool =
  match e with
  | Val _ | Var _ -> false
  | UnOpt (_, v) -> is_symbolic v
  | BinOpt (_, v1, v2) -> List.exists is_symbolic [ v1; v2 ]
  | TriOpt (_, v1, v2, v3) -> List.exists is_symbolic [ v1; v2; v3 ]
  | NOpt (_, es) | Curry (_, es) -> List.exists is_symbolic es
  | Symbolic _ -> true

let rec str (e : t) : string =
  let _str_es es = List.map str es |> String.concat ", " in
  match e with
  | Val v -> Val.str v
  | Var x -> x
  | UnOpt (op, e) -> Operator.str_of_unopt op (str e)
  | BinOpt (op, e1, e2) -> Operator.str_of_binopt op (str e1) (str e2)
  | TriOpt (op, e1, e2, e3) ->
    Operator.str_of_triopt op (str e1) (str e2) (str e3)
  | NOpt (op, es) -> Operator.str_of_nopt op (List.map str es)
  | Curry (f, es) -> Printf.sprintf "{%s}@(%s)" (str f) (_str_es es)
  | Symbolic (t, e') ->
    Printf.sprintf "se_mk_symbolic(%s, %s)" (Type.str t) (str e')

let rec vars_in_expr (e : t) : string list =
  let _vars_in_list lst = List.map vars_in_expr lst |> List.concat in
  match e with
  | Var x -> [ x ]
  | UnOpt (_, e) -> _vars_in_list [ e ]
  | BinOpt (_, e1, e2) -> _vars_in_list [ e1; e2 ]
  | TriOpt (_, e1, e2, e3) -> _vars_in_list [ e1; e2; e3 ]
  | NOpt (_, es) -> _vars_in_list es
  | Curry (e, es) -> _vars_in_list (e :: es)
  | _ -> []

let rec to_json (e : t) : string =
  let to_json_exprs exprs = List.map to_json exprs |> String.concat ", " in
  match e with
  | Val v ->
    Printf.sprintf "{ \"type\" : \"value\", \"value\" : %s }" (Val.to_json v)
  | Var x -> Printf.sprintf "{ \"type\" : \"var\", \"name\" : \"%s\" }" x
  | UnOpt (op, e) ->
    Printf.sprintf "{ \"type\" : \"unop\", \"op\" : %s, \"rhs\" : %s }"
      (Operator.unopt_to_json op)
      (to_json e)
  | BinOpt (op, e1, e2) ->
    Printf.sprintf
      "{ \"type\" : \"binop\", \"op\" : %s, \"lhs\" : %s, \"rhs\" : %s }"
      (Operator.binopt_to_json op)
      (to_json e1) (to_json e2)
  | TriOpt (op, e1, e2, e3) ->
    Printf.sprintf
      "{ \"type\" : \"triop\", \"op\" : %s, \"arg1\" : %s, \"arg2\" : %s, \
       \"arg3\" : %s }"
      (Operator.triopt_to_json op)
      (to_json e1) (to_json e2) (to_json e3)
  | NOpt (op, es) ->
    Printf.sprintf "{ \"type\" : \"nop\", \"op\" : %s, \"args\" : [ %s ]}"
      (Operator.nopt_to_json op) (to_json_exprs es)
  | Curry (f, es) ->
    Printf.sprintf
      "{ \"type\" : \"curry\", \"function:\" : %s, \"args\" : [ %s ]}"
      (to_json f) (to_json_exprs es)
  | Symbolic (t, x) ->
    Printf.sprintf
      "{ \"type\" : \"se_mk_symbolic\", \"val_type\" : \"%s\", \"name\" : \
       \"%s\" }"
      (Type.str t) (str x)

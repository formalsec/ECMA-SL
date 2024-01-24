open Source

type t = t' Source.phrase

and t' =
  | Val of Val.t
  | Var of Id.t'
  | UnOpt of Operator.unopt * t
  | BinOpt of Operator.binopt * t * t
  | TriOpt of Operator.triopt * t * t * t
  | NOpt of Operator.nopt * t list
  | Curry of t * t list
  | Symbolic of Type.t * t

let rec equal (e1 : t) (e2 : t) : bool =
  match (e1.it, e2.it) with
  | (Val v', Val v'') -> Val.equal v' v''
  | (Var x', Var x'') -> x' = x''
  | (UnOpt (op', e'), UnOpt (op'', e'')) -> op' = op'' && equal e' e''
  | (BinOpt (op', e1', e2'), BinOpt (op'', e1'', e2'')) ->
    op' = op'' && equal e1' e1'' && equal e2' e2''
  | (TriOpt (op', e1', e2', e3'), TriOpt (op'', e1'', e2'', e3'')) ->
    op' = op'' && equal e1' e1'' && equal e2' e2'' && equal e3' e3''
  | (NOpt (op', es'), NOpt (op'', es'')) ->
    op' = op'' && List.equal equal es' es''
  | (Curry (fe', es'), Curry (fe'', es'')) ->
    equal fe' fe'' && List.equal equal es' es''
  | (Symbolic (t', e'), Symbolic (t'', e'')) ->
    Type.equal t' t'' && equal e' e''
  | _ -> false

let rec is_symbolic (e : t) : bool =
  match e.it with
  | Val _ | Var _ -> false
  | UnOpt (_, e') -> is_symbolic e'
  | BinOpt (_, e1, e2) -> List.exists is_symbolic [ e1; e2 ]
  | TriOpt (_, e1, e2, e3) -> List.exists is_symbolic [ e1; e2; e3 ]
  | NOpt (_, es) | Curry (_, es) -> List.exists is_symbolic es
  | Symbolic _ -> true

let rec pp (fmt : Fmt.t) (e : t) : unit =
  let open Fmt in
  match e.it with
  | Val v -> Val.pp fmt v
  | Var x -> pp_print_string fmt x
  | UnOpt (op, e') -> Operator.pp_of_unopt pp fmt (op, e')
  | BinOpt (op, e1, e2) -> Operator.pp_of_binopt pp fmt (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.pp_of_triopt pp fmt (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.pp_of_nopt pp fmt (op, es)
  | Curry (fe, es) -> fprintf fmt "{%a}@(%a)" pp fe (pp_lst ", " pp) es
  | Symbolic (t, e') -> fprintf fmt "se_mk_symbolic(%a, %a)" Type.pp t pp e'

let str (e : t) : string = Fmt.asprintf "%a" pp e

let rec vars_in_expr (e : t) : string list =
  let vars_in_lst lst = List.map vars_in_expr lst |> List.concat in
  match e.it with
  | Var x -> [ x ]
  | UnOpt (_, e') -> vars_in_lst [ e' ]
  | BinOpt (_, e1, e2) -> vars_in_lst [ e1; e2 ]
  | TriOpt (_, e1, e2, e3) -> vars_in_lst [ e1; e2; e3 ]
  | NOpt (_, es) -> vars_in_lst es
  | Curry (fe, es) -> vars_in_lst (fe :: es)
  | _ -> []

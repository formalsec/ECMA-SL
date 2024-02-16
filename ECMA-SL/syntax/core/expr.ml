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

let rec pp (fmt : Fmt.t) (e : t) : unit =
  let open Fmt in
  match e.it with
  | Val v -> Val.pp fmt v
  | Var x -> pp_str fmt x
  | UnOpt (op, e') -> Operator.pp_of_unopt pp fmt (op, e')
  | BinOpt (op, e1, e2) -> Operator.pp_of_binopt pp fmt (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.pp_of_triopt pp fmt (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.pp_of_nopt pp fmt (op, es)
  | Curry (fe, es) -> fprintf fmt "{%a}@(%a)" pp fe (pp_lst ", " pp) es
  | Symbolic (t, e') -> (
    match e'.it with
    | Val (Str x) -> fprintf fmt "(`%s : %a)" x Type.pp t
    | _ -> fprintf fmt "(`%a : %a)" pp e' Type.pp t )

let str (e : t) : string = Fmt.asprintf "%a" pp e
let isvoid (e : t) : bool = match e.it with Val Val.Void -> true | _ -> false

let rec vars_in_expr (e : t) : Id.t' list =
  let vars_in_lst lst = List.map vars_in_expr lst |> List.concat in
  match e.it with
  | Var x -> [ x ]
  | UnOpt (_, e') -> vars_in_lst [ e' ]
  | BinOpt (_, e1, e2) -> vars_in_lst [ e1; e2 ]
  | TriOpt (_, e1, e2, e3) -> vars_in_lst [ e1; e2; e3 ]
  | NOpt (_, es) -> vars_in_lst es
  | Curry (fe, es) -> vars_in_lst (fe :: es)
  | _ -> []

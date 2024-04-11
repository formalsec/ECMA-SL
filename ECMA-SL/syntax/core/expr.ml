open Smtml
open EslBase

type t = t' Source.phrase

and t' =
  | Val of Value.t
  | Var of Id.t'
  | UnOpt of Operator.unopt * t
  | BinOpt of Operator.binopt * t * t
  | TriOpt of Operator.triopt * t * t * t
  | NOpt of Operator.nopt * t list
  | Curry of t * t list
  | Symbolic of Type.t * t

let rec pp (ppf : Fmt.t) (e : t) : unit =
  let open Fmt in
  match e.it with
  | Val v -> Value.pp ppf v
  | Var x -> pp_str ppf x
  | UnOpt (op, e') -> Operator.pp_of_unopt pp ppf (op, e')
  | BinOpt (op, e1, e2) -> Operator.pp_of_binopt pp ppf (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.pp_of_triopt pp ppf (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.pp_of_nopt pp ppf (op, es)
  | Curry (fe, es) -> format ppf "{%a}@(%a)" pp fe (pp_lst !>", " pp) es
  | Symbolic (t, e') -> format ppf "se_mk_symbolic(%a, %a)" Type.pp t pp e'

let str (e : t) : string = Fmt.str "%a" pp e
let isvoid (e : t) : bool = match e.it with Val App (`Op "void", []) -> true | _ -> false

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

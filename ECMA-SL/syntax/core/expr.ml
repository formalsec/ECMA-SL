open EslBase
open Source

type t = t' Source.t

and t' =
  | Val of Value.t
  | Var of Id.t'
  | UnOpt of Operator.unopt * t
  | BinOpt of Operator.binopt * t * t
  | TriOpt of Operator.triopt * t * t * t
  | NOpt of Operator.nopt * t list
  | Curry of t * t list
  | Symbolic of Type.t * t

let default : unit -> t =
  let dlft = Val (App (`Op "null", [])) @> none in
  fun () -> dlft

let isvoid (expr : t) : bool =
  match expr.it with Val (App (`Op "void", [])) -> true | _ -> false

let rec pp (ppf : Fmt.t) (expr : t) : unit =
  match expr.it with
  | Val v -> Value.pp ppf v
  | Var x -> Fmt.pp_str ppf x
  | UnOpt (op, e) -> Operator.pp_of_unopt pp ppf (op, e)
  | BinOpt (op, e1, e2) -> Operator.pp_of_binopt pp ppf (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.pp_of_triopt pp ppf (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.pp_of_nopt pp ppf (op, es)
  | Curry (fe, es) -> Fmt.fmt ppf "{%a}@(%a)" pp fe Fmt.(pp_lst !>", " pp) es
  | Symbolic (t, e) -> Fmt.fmt ppf "se_mk_symbolic(%a, %a)" Type.pp t pp e

let str (expr : t) : string = Fmt.str "%a" pp expr [@@inline]

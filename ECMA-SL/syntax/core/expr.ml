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

let default : unit -> t =
  let dlft = Val Value.null @> none in
  fun () -> dlft

let isvoid (e : t) : bool = match e.it with Val Unit -> true | _ -> false

let rec pp (ppf : Format.formatter) (e : t) : unit =
  match e.it with
  | Val v -> Value.pp ppf v
  | Var x -> Fmt.string ppf x
  | UnOpt (op, e') -> Operator.unopt_pp ~pp_v:pp ppf (op, e')
  | BinOpt (op, e1, e2) -> Operator.binopt_pp ~pp_v:pp ppf (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.triopt_pp ~pp_v:pp ppf (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.nopt_pp ~pp_v:pp ppf (op, es)
  | Curry (fe, es) ->
    Fmt.pf ppf "@[<h>{%a}@(%a)@]" pp fe Fmt.(list ~sep:comma pp) es

let str (e : t) : string = Fmt.str "%a" pp e [@@inline]

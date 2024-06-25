module type M = sig
  type t

  exception Except of string

  val top : t option ref
  val flows : (t * t) list ref
  val setTop : t -> unit
  val addFlow : t -> t -> unit
  val str : t -> string
  val parse_lvl : string -> t
  val lub : t -> t -> t
  val lubn : t list -> t
  val leq : t -> t -> bool
  val get_low : unit -> t
  val get_high : unit -> t
end

exception Except of string

type t =
  | High
  | Low

type flow = t * t

let top : t option ref = ref None
let flows : flow list ref = ref []
let all_levels : t list ref = ref []

let addFlow (_lst1 : t) (_lst2 : t) : unit =
  raise (Except "Illegal Lattice operation - addflow")

let setTop (_ : t) : unit = raise (Except "Illegal Lattice operation - setTop")
let str (l : t) = match l with High -> "high" | Low -> "low"
let get_low () : t = Low
let get_high () : t = High

let parse_lvl (str : string) : t =
  if str = "low" then Low
  else if str = "high" then High
  else raise (Except ("Unknown Level -" ^ str))

let lub (l1 : t) (l2 : t) : t =
  match (l1, l2) with (High, _) | (_, High) -> High | (_, _) -> Low

let lubn (lst : t list) : t = List.fold_left lub Low lst

let leq (l1 : t) (l2 : t) : bool =
  match (l1, l2) with (_, High) -> true | (Low, _) -> true | (_, _) -> false

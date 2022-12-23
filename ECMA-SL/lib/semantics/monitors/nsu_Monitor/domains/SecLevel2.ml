open Common

exception Except of string

type t = High | MidOne | MidTwo | Low

let top : SSet.t option ref = raise (Except "Illegal Lattice operation ")

let flows : (SSet.t * SSet.t) list ref =
  raise (Except "Illegal Lattice operation ")

let addFlow (_ : t) (_ : t) : unit = raise (Except "Illegal Lattice operation ")
let setTop (_ : t) : unit = raise (Except "Illegal Lattice operation ")

let str (l : t) =
  match l with
  | High -> "high"
  | MidOne -> "midone"
  | MidTwo -> "midtwo"
  | Low -> "low"

let get_low () : t = Low
let get_high () : t = High

let parse_lvl (str : string) : t =
  if str = "low" then Low
  else if str = "high" then High
  else if str = "midone" then MidOne
  else if str = "midtwo" then MidTwo
  else raise (Except ("Unknown Level -" ^ str))

let lub (l1 : t) (l2 : t) : t =
  match (l1, l2) with
  | High, _ | _, High | MidOne, MidTwo | MidTwo, MidOne -> High
  | MidOne, _ | _, MidOne -> MidOne
  | MidTwo, _ | _, MidTwo -> MidTwo
  | _, _ -> Low

let lubn (lst : t list) : t = List.fold_left lub Low lst

let leq (l1 : t) (l2 : t) : bool =
  match (l1, l2) with
  | _, High -> true
  | Low, _ -> true
  | MidOne, MidOne -> true
  | MidTwo, MidTwo -> true
  | _, _ -> false

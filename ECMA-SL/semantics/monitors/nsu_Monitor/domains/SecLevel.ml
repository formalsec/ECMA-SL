exception Except of string



type t =
  | High
  | Low

type flow = t * t

let top  : t option ref = ref None

let flows : flow list ref = ref []

let all_levels : t list ref = ref []

let addFlow (lst1 : t) (lst2 : t) : unit = 
  raise (Except "Illegal Lattice operation - addflow")

let setTop (_ : t): unit = 
  raise (Except "Illegal Lattice operation - setTop")

let str (l:t) =
  match l with
  | High -> "high"
  | Low -> "low"

let get_low () : t = Low  

let get_high () : t = High 

let parse_lvl (str:string) : t =
  if (str = "low") then Low
  else if(str = "high") then High
  else raise(Except ("Unknown Level -"^str))


let lub (l1:t) (l2:t): t =
  match l1, l2 with
  | High, _
  | _, High-> High
  | _, _ -> Low

let lubn (lst : t list): t =
  List.fold_left lub Low lst


let leq (l1:t) (l2:t): bool =
  match l1,l2 with
  |_, High ->true
  |Low, _ -> true
  |_, _ -> false

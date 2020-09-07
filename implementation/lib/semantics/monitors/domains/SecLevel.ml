exception Except of string

type t=
  | High
  | Low


let str (l:t) =
  match l with
  | High -> "high"
  | Low -> "low"



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

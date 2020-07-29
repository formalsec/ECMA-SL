exception Except of string

type sf =
  |Intermediate of (Level.t list * SecStore.t * string)
  |Toplevel

type t = sf list

let pop (cs: t): (sf * t) =
  let cs' = List.rev cs in
  match cs' with
  | [] -> (raise(Except "The Security stack is empty already!")(*ERROR*))
  | f::frames -> (f,frames)

;;


let push (cs: t) (frame:sf) : t =
  let cs' = List.rev cs in
  let cs'' = (frame :: cs') in
  let finalcs = List.rev cs'' in

  finalcs
;;

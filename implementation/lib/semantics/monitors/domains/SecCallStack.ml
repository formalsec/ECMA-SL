exception Except of string

type sf =
  |Intermediate of (SecLevel.t list * SecStore.t * string)
  |Toplevel

type t = sf list

let create () : t =
  [Toplevel]

let pop (cs: t): (sf * t) =
  match cs with
  | [] -> (raise(Except "The Security stack is empty already!")(*ERROR*))
  | f::frames -> (f,frames)

;;


let push (cs: t) (frame:sf) : t =
  let cs' = (frame :: cs) in
  cs'
;;

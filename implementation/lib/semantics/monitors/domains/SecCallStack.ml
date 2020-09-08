exception Except of string

type 'sl sf =
  |Intermediate of ('sl list * ('sl SecStore.t) * string)
  |Toplevel

type 'sl t = ('sl sf) list

let create () : 'sl t =
  [Toplevel]

let pop (cs: ('sl t)) : (('sl sf) * ('sl t)) =
  match cs with
  | [] -> (raise(Except "The Security stack is empty already!")(*ERROR*))
  | f::frames -> (f,frames)

;;


let push (cs: ('sl t)) (frame: ('sl sf)) : ('sl t) =
  let cs' = (frame :: cs) in
  cs'
;;

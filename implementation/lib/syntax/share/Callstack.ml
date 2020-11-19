exception Except of string

type sft =
  | Intermediate of (Stmt.t list * Store.t * string * string)
  | Toplevel

type t = sft list

let pop (cs: t): (sft * t) =
  match cs with
  | [] -> (raise(Except "The stack is Empty already!")(*ERROR*))
  | f::frames -> (f,frames)
;;

let push (cs: t) (frame:sft) : t =
  let cs' = (frame :: cs) in
  cs'
;;

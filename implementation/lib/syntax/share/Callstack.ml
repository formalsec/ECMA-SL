exception Except of string

type sft =
  | Intermediate of (Stmt.t list * Store.t * string)
  | Toplevel

type t = sft list

let pop (cs: t): (sft * t) =
  let cs' = List.rev cs in
  match cs' with
  | [] -> (raise(Except "The stack is Empty already!")(*ERROR*))
  | f::frames -> (f,frames)
;;

let push (cs: t) (frame:sft) : t =
  let cs' = List.rev cs in
  let cs'' = (frame :: cs') in
  let finalcs = List.rev cs'' in
  finalcs
;;

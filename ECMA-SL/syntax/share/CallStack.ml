type sft =
  | Intermediate of (Stmt.t list * Store.t * string * string)
  | Toplevel

type t = sft list

(* Perhaps empty would be better? *)
exception Except of string

let empty : t = []

let pop (cs : t) : sft * t =
  match cs with
  (* Maybe raise Empty ? instead of a Except*)
  | [] -> raise (Except "The stack is Empty already!")
  | f :: frames -> (f, frames)

let str_sft (frame : sft) : string =
  match frame with Intermediate (_, _, _, f) -> f | Toplevel -> "TopLevel"

let str (cs : t) : string = String.concat "; " (List.map str_sft cs)
let push (cs : t) (frame : sft) : t = frame :: cs

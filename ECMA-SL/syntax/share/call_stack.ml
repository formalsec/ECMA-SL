type func = string

type 'a sft =
  | Intermediate of (Stmt.t list * 'a * string * func)
  | Toplevel

type 'a t = 'a sft list

exception Empty_stack

let empty : 'a t = []

let pop (cs : 'a t) : 'a sft * 'a t =
  match cs with
  | [] -> raise Empty_stack
  | f :: frames -> (f, frames)

let str_sft (frame : 'a sft) : string =
  match frame with
  | Intermediate (_, _, _, f) -> f
  | Toplevel -> "TopLevel"

let str (cs : 'a t) : string = String.concat "; " (List.map str_sft cs)
let push (cs : 'a t) (frame : 'a sft) : 'a t = frame :: cs

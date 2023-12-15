type func = string

type 'a sft =
  | Intermediate of (Stmt.t list * 'a * string * func)
  | Toplevel

type 'a t = 'a sft list

exception Empty_stack

let empty : 'a t = []

let str (cs : 'a t) : string =
  let str_frame_fun = function
    | Intermediate (_, _, _, f) -> f
    | Toplevel -> "TopLevel"
  in
  List.map str_frame_fun cs |> String.concat "; "

let pop (cs : 'a t) : 'a sft * 'a t =
  match cs with
  | [] -> raise Empty_stack
  | f :: frames -> (f, frames)

let push (cs : 'a t) (frame : 'a sft) : 'a t = frame :: cs

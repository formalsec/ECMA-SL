type func = string

type 'a sft =
  | Toplevel
  | Intermediate of (Stmt.t list * 'a * string * func)

type 'a t = 'a sft list

exception Empty_stack

let empty : 'a t = []

let str (stack : 'a t) : string =
  let str_frame_fun = function
    | Toplevel -> "TopLevel"
    | Intermediate (_, _, _, f) -> f
  in
  List.map str_frame_fun stack |> String.concat "; "

let pop (stack : 'a t) : 'a sft * 'a t =
  match stack with [] -> raise Empty_stack | f :: frames -> (f, frames)

let push (stack : 'a t) (frame : 'a sft) : 'a t = frame :: stack

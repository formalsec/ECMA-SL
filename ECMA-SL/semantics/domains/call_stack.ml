type retvar = string
type func = Func.t

type 'store frame =
  | Toplevel
  | Intermediate of (Stmt.t list * 'store * retvar * func)

type 'store t = 'store frame list

exception Empty_stack

let empty : 'store t = []

let pop (stack : 'store t) : 'store frame * 'store t =
  match stack with [] -> raise Empty_stack | frame :: stack' -> (frame, stack')

let push (stack : 'store t) (frame : 'store frame) : 'store t = frame :: stack

let str (stack : 'store t) : string =
  let str_frame_f = function
    | Toplevel -> "TopLevel"
    | Intermediate (_, _, _, func) -> Func.name func
  in
  List.map str_frame_f stack |> String.concat " -> "

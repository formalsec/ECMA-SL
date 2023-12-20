type 'sl sf =
  | Intermediate of ('sl list * 'sl NSUStore.t * string)
  | Toplevel

type 'sl t = 'sl sf list

let create () : 'sl t = [ Toplevel ]

let pop (cs : 'sl t) : 'sl sf * 'sl t =
  match cs with
  | [] -> raise (NSUException.Except "The Security Call Stack is empty")
  | f :: frames -> (f, frames)

let push (cs : 'sl t) (frame : 'sl sf) : 'sl t = frame :: cs

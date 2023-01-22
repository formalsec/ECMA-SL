type 'a sft = Intermediate of (Stmt.t list * 'a * string * string) | Toplevel
type 'a t

exception Empty_stack

val empty : 'a t
val pop : 'a t -> 'a sft * 'a t
val push : 'a t -> 'a sft -> 'a t
val str : 'a t -> string

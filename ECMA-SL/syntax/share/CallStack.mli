type sft =
  | Intermediate of (Stmt.t list * Store.t * string * string)
  | Toplevel

type t 

val empty : t 

val pop : t -> (sft * t)

val push :  t -> sft -> t

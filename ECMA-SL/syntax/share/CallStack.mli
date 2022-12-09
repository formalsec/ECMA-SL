type sft =
  | Intermediate of (Stmt.t list * Store.t * string)
  | Toplevel

type t 

val pop : t ->  t

val push :  t -> sft -> t

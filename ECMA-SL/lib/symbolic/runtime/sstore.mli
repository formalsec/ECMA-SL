type bind = String.t
type t

val create : (string * Expr.t) list -> t
val mem : t -> bind -> bool
val add_exn : t -> bind -> Expr.t -> t
val find : t -> bind -> Expr.t option
val to_string : t -> string

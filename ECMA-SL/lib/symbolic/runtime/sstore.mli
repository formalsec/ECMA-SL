type bind = string
type t

val create : (string * Sval.t) list -> t
val mem : t -> bind -> bool
val add : t -> bind -> Sval.t -> t
val find_opt : t -> bind -> Sval.t option
val to_string : t -> string

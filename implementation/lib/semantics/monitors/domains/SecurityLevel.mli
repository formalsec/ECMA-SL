module type M = sig 

type t

val str : t -> string 

val parse_lvl : string -> t 

val lub : t -> t -> t 

val lubn : t list -> t 

val leq : t -> t -> bool 

val get_low : unit -> t 

val get_high : unit -> t 

end
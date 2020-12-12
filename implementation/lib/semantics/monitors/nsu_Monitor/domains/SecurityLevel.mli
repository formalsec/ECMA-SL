module type M = sig 

type t

val top :  t option ref 

val flows :  (t * t) list ref 

val setTop : t  -> unit

val addFlow : t -> t -> unit

val str : t -> string 

val parse_lvl : string -> t 

val lub : t -> t -> t 

val lubn : t list -> t 

val leq : t -> t -> bool 

val get_low : unit -> t 

val get_high : unit -> t 

end
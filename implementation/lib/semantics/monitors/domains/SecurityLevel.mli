module type M = sig 

type t

val top : unit -> t option ref 

val flows : unit -> (t * t) list ref 

val setTop : string list -> unit

val addFlow : string list -> string list ->unit

val str : t -> string 

val parse_lvl : string -> t 

val lub : t -> t -> t 

val lubn : t list -> t 

val leq : t -> t -> bool 

val get_low : unit -> t 

val get_high : unit -> t 

end
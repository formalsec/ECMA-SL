
type 'sl t

val create : (string * 'sl) list -> 'sl t

val get : 'sl t -> string -> 'sl 

val set : 'sl t -> string -> 'sl -> unit 

val get_safe : 'sl t -> string -> 'sl option

val str : ('sl -> string) -> 'sl t -> string 


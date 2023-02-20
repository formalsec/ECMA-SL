type var = string
type 'a t

val create : (var * 'a) list -> 'a t
val clone : 'a t -> 'a t
val get : 'a t -> var -> 'a option
val set : 'a t -> var -> 'a -> unit
val str : 'a t -> ('a -> string) -> string

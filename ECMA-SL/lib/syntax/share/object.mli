type 'a t

val create : unit -> 'a t
val clone : 'a t -> 'a t
val get : 'a t -> Field.t -> 'a option
val set : 'a t -> Field.t -> 'a -> unit
val delete : 'a t -> Field.t -> unit
val to_list : 'a t -> (Field.t * 'a) list
val to_string : 'a t -> ('a -> string) -> string
val to_json : 'a t -> ('a -> string) -> string
val get_fields : 'a t -> Field.t list

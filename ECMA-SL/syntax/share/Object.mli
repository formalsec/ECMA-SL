type t

val create : unit -> t 
val get    : t -> Field.t -> Val.t option 
val set    : t -> Field.t -> Val.t -> unit
val delete : t -> Field.t -> unit

val to_list    : t -> (Field.t * Val.t) list
val get_fields : t -> Field.t list 

val str     : t -> string 
val to_json : t -> string 

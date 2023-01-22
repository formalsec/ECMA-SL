type 'sl t

val create : unit -> 'sl t
val get : 'sl t -> Field.t -> ('sl * 'sl) option
val set : 'sl t -> Field.t -> 'sl -> 'sl -> unit
val delete : 'sl t -> Field.t -> unit
val str : ('sl -> string) -> 'sl t -> string
val new_sec_prop : 'sl t -> Field.t -> 'sl -> 'sl -> unit
val upg_exists : 'sl t -> Field.t -> 'sl -> unit
val upg_val : 'sl t -> Field.t -> 'sl -> unit

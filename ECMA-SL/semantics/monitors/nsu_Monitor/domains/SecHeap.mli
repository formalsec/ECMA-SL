type 'sl t

val create : unit -> 'sl t
val insert : 'sl t -> Loc.t -> 'sl SecObject.t -> 'sl -> 'sl -> unit
val create_object : 'sl t -> Loc.t -> 'sl -> unit
val delete : 'sl t -> Loc.t -> unit
val update : 'sl t -> Loc.t -> 'sl SecObject.t -> 'sl -> 'sl -> unit
val get : 'sl t -> Loc.t -> 'sl SecObject.t * 'sl * 'sl
val get_struct_lvl : 'sl t -> Loc.t -> 'sl option
val get_object_lvl : 'sl t -> Loc.t -> 'sl option
val get_obj : 'sl t -> Loc.t -> 'sl SecObject.t option
val get_field : 'sl t -> Loc.t -> Field.t -> ('sl * 'sl) option
val delete_field : 'sl t -> Loc.t -> Field.t -> bool
val new_sec_prop : 'sl t -> Loc.t -> Field.t -> 'sl -> 'sl -> bool
val newSecObj : 'sl t -> Loc.t -> 'sl -> 'sl -> unit
val upg_prop_exists_lvl : 'sl t -> Loc.t -> Field.t -> 'sl -> unit
val upg_prop_val_lvl : 'sl t -> Loc.t -> Field.t -> 'sl -> unit
val upg_object_lvl : 'sl t -> Loc.t -> 'sl -> unit
val upg_struct_lvl : 'sl t -> Loc.t -> 'sl -> unit
val str : ('sl -> string) -> 'sl t -> string

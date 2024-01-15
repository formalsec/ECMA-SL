(* TODO: merge these type signatures *)
module type S = sig
  type t
  type value
  type object_

  val create : unit -> t
  val clone : t -> t
  val insert : t -> object_ -> value
  val remove : t -> Loc.t -> unit
  val set : t -> Loc.t -> object_ -> unit
  val get : t -> Loc.t -> object_ option
  val set_field : t -> Loc.t -> field:value -> data:value -> unit
  val get_field : t -> Loc.t -> value -> (value * value list) list
  val has_field : t -> Loc.t -> value -> value
  val delete_field : t -> Loc.t -> value -> unit
  val pp : Format.formatter -> t -> unit
  val loc : value -> ((value option * string) list, string) Result.t
  val pp_val : t -> value -> string
end

module type S2 = sig
  type t
  type value
  type value2
  type object_

  val create : unit -> t
  val clone : t -> t
  val insert : t -> object_ -> value
  val remove : t -> Loc.t -> unit
  val set : t -> Loc.t -> object_ -> unit
  val get : t -> Loc.t -> object_ option

  val set_field :
       t
    -> Loc.t
    -> field:value
    -> data:value
    -> Solver.t
    -> value2 list
    -> (t * value2 list) list

  val get_field :
       t
    -> Loc.t
    -> value
    -> Solver.t
    -> value2 list
    -> (t * value2 list * value option) list

  val has_field :
       t
    -> Loc.t
    -> value
    -> Solver.t
    -> value2 list
    -> (t * value2 list * value) list

  val delete_field :
    t -> Loc.t -> value -> Solver.t -> value2 list -> (t * value2 list) list
end

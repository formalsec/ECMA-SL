(* TODO: merge these type signatures *)
module type S = sig
  type t
  type value

  val create : unit -> t
  val clone : t -> t
  val is_empty : t -> bool
  val to_list : t -> (value * value) list
  val get_fields : t -> value list
  val has_field : t -> value -> value
  val set : t -> key:value -> data:value -> t
  val get : t -> value -> (value * value list) list
  val delete : t -> value -> t
  val pp : t Fmt.t
  val to_string : t -> string
  val to_json : t -> string
end

module type S2 = sig
  type t
  type value
  type value2

  val create : unit -> t
  val clone : t -> t
  val is_empty : t -> bool
  val to_list : t -> (value * value) list
  val has_field : t -> value -> value
  val get_fields : t -> value list

  val set :
       t
    -> key:value
    -> data:value
    -> Solver.t
    -> value2 list
    -> (t * value2 list) list

  val get :
       t
    -> value
    -> Solver.t
    -> value2 list
    -> (t * value2 list * value option) list

  val delete : t -> value -> Solver.t -> value2 list -> (t * value2 list) list
end

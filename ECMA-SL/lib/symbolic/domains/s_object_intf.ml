module type S = sig
  module V : Value_intf.T
  type t
  type encoded_pct = Encoding.Expression.t

  val create : unit -> t
  val clone : t -> t
  val to_string : t -> (V.value -> string) -> string

  val set :
    t ->
    V.value ->
    V.value ->
    Batch.t ->
    encoded_pct list ->
    (t * encoded_pct list) list

  val get :
    t ->
    V.value ->
    Batch.t ->
    encoded_pct list ->
    (t * encoded_pct list * V.value option) list

  val delete :
    t -> V.value -> Batch.t -> encoded_pct list -> (t * encoded_pct list) list

  val to_list : t -> (V.value * V.value) list
  val has_field : t -> V.value -> V.value
  val get_fields : t -> V.value list
end

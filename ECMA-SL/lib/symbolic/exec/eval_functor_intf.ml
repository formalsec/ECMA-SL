module type P = sig
  module Store : sig
    type bind = String.t
    type t

    val create : (string * Expr.t) list -> t
    val mem : t -> bind -> bool
    val add_exn : t -> bind -> Expr.t -> t
    val find : t -> bind -> Expr.t option
    val to_string : t -> string
  end

  module Object : S_object_intf.S

  module Heap : sig
    type encoded_pct = Encoding.Expression.t
    type obj = Object.t
    type t

    val create : unit -> t
    val clone : t -> t
    val insert : t -> obj -> Loc.t
    val remove : t -> Loc.t -> unit
    val set : t -> Loc.t -> obj -> unit
    val get : t -> Loc.t -> obj option
    val has_field : t -> Loc.t -> Expr.t -> Expr.t

    val get_field :
      t ->
      Loc.t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      (t * encoded_pct list * Expr.t option) list

    val set_field :
      t ->
      Loc.t ->
      Expr.t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      (t * encoded_pct list) list

    val delete_field :
      t ->
      Loc.t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      (t * encoded_pct list) list
  end

  module Reducer : sig
    val reduce_expr : Expr.t -> Expr.t
  end
end

module type S = sig
  module State : sig
    type store
    type env
    type solver
    type optimizer
    type exec_state
 end

  val main : Prog.t -> string -> unit
end

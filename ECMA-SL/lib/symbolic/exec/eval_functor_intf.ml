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

  module Object : sig
    type t
    type encoded_pct = Encoding.Expression.t

    val create : unit -> t
    val clone : t -> t
    val to_string : t -> (Expr.t -> string) -> string

    val set :
      t ->
      Expr.t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      S_store.t ->
      (t * encoded_pct list) list

    val get :
      t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      S_store.t ->
      (t * encoded_pct list * Expr.t option) list

    val delete :
      t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      S_store.t ->
      (t * encoded_pct list) list

    val to_list : t -> (Expr.t * Expr.t) list
    val has_field : t -> Expr.t -> Expr.t
    val get_fields : t -> Expr.t list
  end

  module Heap : sig
    type encoded_pct = Encoding.Expression.t
    type obj = Object.t
    type store = Store.t
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
      store ->
      (t * encoded_pct list * Expr.t option) list

    val set_field :
      t ->
      Loc.t ->
      Expr.t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      store ->
      (t * encoded_pct list) list

    val delete_field :
      t ->
      Loc.t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      store ->
      (t * encoded_pct list) list
  end

  module Reducer : sig
    val reduce_expr : Expr.t -> Expr.t
  end
end

module type S = sig
  module State : sig
    type pc
    type func
    type stack
    type state

    type outcome =
      | Cont of Stmt.t list
      | Error of Expr.t option
      | Final of Expr.t option
      | Failure of string * Expr.t option
      | Unknown of Expr.t option

    type config

    val is_cont : outcome -> bool
    val is_fail : outcome -> bool
    val is_final : outcome -> bool
    val update : config -> outcome -> state -> pc -> config
  end

  val main : Prog.t -> string -> unit
end

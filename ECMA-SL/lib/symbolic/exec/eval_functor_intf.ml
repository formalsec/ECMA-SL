module type P = sig
  type value
  type env
  type memory
  type object_
  type store

  module Store : sig
    type bind = String.t
    type t = store

    val create : (string * Expr.t) list -> t
    val mem : t -> bind -> bool
    val add_exn : t -> bind -> Expr.t -> t
    val find : t -> bind -> Expr.t option
    val to_string : t -> string
  end

  module Object : sig
    type t = object_
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
      (t * encoded_pct list) list

    val get :
      t ->
      Expr.t ->
      Batch.t ->
      encoded_pct list ->
      (t * encoded_pct list * Expr.t option) list

    val delete :
      t -> Expr.t -> Batch.t -> encoded_pct list -> (t * encoded_pct list) list

    val to_list : t -> (Expr.t * Expr.t) list
    val has_field : t -> Expr.t -> Expr.t
    val get_fields : t -> Expr.t list
  end

  module Heap : sig
    type encoded_pct = Encoding.Expression.t
    type obj = object_
    type t = memory

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

  module Env : sig
    type t = env
    type nonrec memory = memory

    val clone : t -> t
    val get_memory : t -> memory
    val get_func : t -> string -> (Func.t, string) Result.t

    val get_extern_func :
      t -> string -> (Extern_func.extern_func, string) Result.t

    val add_memory : t -> memory -> t
  end

  module Reducer : sig
    val reduce_expr : Expr.t -> Expr.t
  end
end

module type S = sig
  type env
  type value

  module State : sig
    type store
    type env
    type solver
    type optimizer
    type exec_state
  end

  val main : env -> string -> unit
end

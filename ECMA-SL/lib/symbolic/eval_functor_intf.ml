module type P = sig
  type value
  type env
  type memory
  type object_
  type store

  module Value : Value_intf.T with type value = value and type store = store

  module Choice : sig
    val assertion : Batch.t -> Encoding.Expression.t list -> value -> bool
    val assumption : value -> bool option

    val branch :
      Batch.t ->
      Encoding.Expression.t list ->
      value ->
      (bool * Encoding.Expression.t option)
      * (bool * Encoding.Expression.t option)
  end

  module Store : sig
    type bind = string
    type t = store

    val create : (string * value) list -> t
    val mem : t -> bind -> bool
    val add_exn : t -> bind -> value -> t
    val find : t -> bind -> value option
  end

  module Object : sig
    type t = object_
    type nonrec value = value
    type encoded_pct = Encoding.Expression.t

    val create : unit -> t
    val clone : t -> t
    val to_string : t -> (value -> string) -> string

    val set :
      t ->
      value ->
      value ->
      Batch.t ->
      encoded_pct list ->
      (t * encoded_pct list) list

    val get :
      t ->
      value ->
      Batch.t ->
      encoded_pct list ->
      (t * encoded_pct list * value option) list

    val delete :
      t -> value -> Batch.t -> encoded_pct list -> (t * encoded_pct list) list

    val to_list : t -> (value * value) list
    val has_field : t -> value -> value
    val get_fields : t -> value list
  end

  module Heap : sig
    type encoded_pct = Encoding.Expression.t
    type obj = object_
    type t = memory
    type nonrec value = value

    val create : unit -> t
    val clone : t -> t
    val insert : t -> obj -> Loc.t
    val remove : t -> Loc.t -> unit
    val set : t -> Loc.t -> obj -> unit
    val get : t -> Loc.t -> obj option
    val has_field : t -> Loc.t -> value -> value

    val get_field :
      t ->
      Loc.t ->
      value ->
      Batch.t ->
      encoded_pct list ->
      (t * encoded_pct list * value option) list

    val set_field :
      t ->
      Loc.t ->
      value ->
      value ->
      Batch.t ->
      encoded_pct list ->
      (t * encoded_pct list) list

    val delete_field :
      t ->
      Loc.t ->
      value ->
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
    val reduce : value -> value
  end

  module Translator : sig
    val expr_of_value : Encoding.Value.t -> value
    val translate : ?b:bool -> value -> Encoding.Expression.t
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

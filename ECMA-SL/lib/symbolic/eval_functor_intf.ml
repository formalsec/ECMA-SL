module type P = sig
  type value
  type env
  type memory
  type object_
  type store

  module Value : Value_intf.T with type value = value and type store = store
  module Extern_func : Extern_func.T with type value = value

  module Choice : Choice_monad_intf.Base with module V := Value

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

    val create : unit -> t
    val to_list : t -> (value * value) list
    val get_fields : t -> value list
    val has_field : t -> value -> value
    val set : t -> key:value -> data:value -> t
    val get : t -> value -> (value * value list) list
    val delete : t -> value -> t
    val to_string : t -> string
  end

  module Heap : sig
    type t = memory
    type nonrec value = value
    type nonrec object_ = object_

    val create : unit -> t
    val clone : t -> t
    val insert : t -> object_ -> value
    val remove : t -> Loc.t -> unit
    val set : t -> Loc.t -> object_ -> unit
    val get : t -> Loc.t -> object_ option
    val has_field : t -> Loc.t -> value -> value
    val set_field : t -> Loc.t -> field:value -> data:value -> unit
    val get_field : t -> Loc.t -> value -> (value * value list) list
    val delete_field : t -> Loc.t -> value -> unit
    val to_string : t -> string
    val loc : value -> string Choice.t
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
  type 'a choice

  module State : sig
    type store
    type env
    type exec_state
  end

  val main : env -> string -> unit choice
end

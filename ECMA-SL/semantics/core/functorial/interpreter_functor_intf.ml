open EslBase
open EslSyntax

module type P = sig
  type value
  type env
  type memory
  type object_
  type store

  module Value : Value_intf.T with type value = value and type store = store
  module Choice : Choice_monad_intf.Complete with module V := Value

  module Extern_func :
    Extern_func.T with type value = value with type 'a choice := 'a Choice.t

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
    val get : t -> value -> value option Choice.t
    val delete : t -> value -> t
    val to_string : t -> string
  end

  module Memory : sig
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
    val get_field : t -> Loc.t -> value -> value option Choice.t
    val delete_field : t -> Loc.t -> value -> unit
    val pp : Fmt.formatter -> t -> unit
    val to_string : t -> string
    val loc : value -> Loc.t Choice.t
    val pp_val : t -> value -> string
  end

  module Env : sig
    type t = env
    type nonrec memory = memory

    val clone : t -> t
    val get_memory : t -> memory Choice.t
    val get_func : t -> string -> (Func.t, string) Result.t

    val get_extern_func :
      t -> string -> (Extern_func.extern_func, string) Result.t

    val add_memory : t -> memory -> t
    val add_func : t -> string -> Func.t -> unit
  end
end

module type S = sig
  type env
  type value
  type 'a choice

  module State : sig
    type store
    type env

    type err =
      [ `Abort of string
      | `Assert_failure of value
      | `Failure of string
      ]

    type return_result = (value list, err) Result.t
    type exec_state
  end

  val main : env -> string -> State.return_result choice
end

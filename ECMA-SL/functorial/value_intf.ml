module type T = sig
  type value
  type store

  val mk_symbol : string -> value
  val mk_list : value list -> value
  val mk_tuple : value * value -> value
  val is_symbolic : value -> bool
  val equal : value -> value -> bool
  val get_func_name : value -> (string * value list, string) Result.t

  module Bool : sig
    val const : bool -> value
    val not_ : value -> value
    val and_ : value -> value -> value
    val or_ : value -> value -> value
  end

  module Store : sig
    type bind = string
    type t = store

    val create : (bind * value) list -> t
    val mem : t -> bind -> bool
    val add_exn : t -> bind -> value -> t
    val find : t -> bind -> value option
  end

  val eval_expr : store -> Expr.t -> (value, string) Result.t

  module Pp : sig
    val pp : Format.formatter -> value -> unit

    module Store : sig
      val pp : Fmt.t -> store -> unit
    end
  end
end

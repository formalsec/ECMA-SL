open EslBase
open EslSyntax

module type T = sig
  type value
  type store

  val equal : value -> value -> bool
  val hash : value -> int
  val compare : value -> value -> int
  val pp : Fmt.t -> value -> unit
  val mk_symbol : string -> value
  val mk_list : value list -> value
  val mk_tuple : value * value -> value
  val is_symbolic : value -> bool
  val func : value -> (string * value list, string) Result.t

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
    val pp : Fmt.t -> t -> unit
  end

  val eval_expr : store -> Expr.t -> value
end

module type M = sig
  type t

  val create : string list -> t
  val empty : unit -> t
end

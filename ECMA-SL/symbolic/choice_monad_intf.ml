module type Base = sig
  module V : Value_intf.T

  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val check : V.value -> bool t
  val check_add_true : V.value -> bool t
  val branch : V.value -> bool t
  val error : string -> 'a t
end

module type Complete = sig
  include Base

  type thread

  val run : 'a t -> thread -> ('a * thread) list
end

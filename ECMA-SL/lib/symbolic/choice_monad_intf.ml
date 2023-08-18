module type Base = sig
  module V : Value_intf.T

  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val select : V.value -> bool t
  val branch : V.value -> bool t
  val error : string -> 'a t
end

module type Complete = sig
  include Base

  type thread

  val run : 'a t -> thread -> ('a * thread) list
end

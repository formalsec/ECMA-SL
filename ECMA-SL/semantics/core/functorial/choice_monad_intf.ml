module type Base = sig
  module V : Value_intf.T

  (* TODO: rename to state? *)
  type thread
  type 'a t

  val return : 'a -> 'a t
  val stop : 'a t
  val run : 'a t -> thread -> ('a * thread) EslBase.Cont.t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val with_thread : (thread -> 'a) -> 'a t
end

module type Complete = sig
  include Base

  val check : V.value -> bool t
  val check_add_true : V.value -> bool t
  val branch : V.value -> bool t
  val select_val : V.value -> Smtml.Value.t t
end

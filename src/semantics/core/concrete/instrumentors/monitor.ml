open EslSyntax
include Monitor_intf

module type M = sig
  type sl
  type state
  type return
  type 'a label

  type t =
    { mutable state : state
    ; mutable label : sl label
    }

  val initial_state : unit -> t
  val set_label : t -> sl label -> unit
  val update_label : t -> Stmt.t -> stmt_eval -> unit
  val eval_small_step : t -> unit
  val interceptor : string -> Value.t list -> Expr.t list -> sl label option
end

module Default : M = struct
  type sl = unit
  type state = unit
  type return = unit
  type 'a label = unit

  type t =
    { mutable state : state
    ; mutable label : sl label
    }

  let initial_state () : t = { state = (); label = () }
  let set_label (mon : t) (label : sl label) : unit = mon.label <- label
  let update_label (_ : t) (_ : Stmt.t) (_ : stmt_eval) : unit = ()
  let eval_small_step (_ : t) : unit = ()

  let interceptor (_ : string) (_ : Value.t list) (_ : Expr.t list) :
    sl label option =
    None
end

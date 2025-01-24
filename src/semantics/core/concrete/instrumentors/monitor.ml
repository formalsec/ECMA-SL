open EslSyntax
include Monitor_intf

module Nop : Monitor_intf.S = struct
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

module Printer : Monitor_intf.S = struct
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

  let update_label (_ : t) (s : Stmt.t) (label : stmt_eval) : unit =
    Fmt.pr "Stmt: %a, Label: %a@." Stmt.pp_simple s pp_stmt_eval label;
    match (label, Stmt.view s) with
    | (AssignEval, Assign (_id, _e)) -> ()
    | _ -> ()

  let eval_small_step (_ : t) : unit = ()

  let interceptor (_ : string) (_ : Value.t list) (_ : Expr.t list) :
    sl label option =
    None
end

module Other : Monitor_intf.S = struct
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

  let update_label (_ : t) (s : Stmt.t) (label : stmt_eval) : unit =
    Fmt.pr "Stmt: %a, Label: %a@." Stmt.pp_simple s pp_stmt_eval label

  let eval_small_step (_ : t) : unit = ()

  let interceptor (_ : string) (_ : Value.t list) (_ : Expr.t list) :
    sl label option =
    None
end

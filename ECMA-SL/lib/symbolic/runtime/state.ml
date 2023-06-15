open Core
module ESet = Set.Make (Encoding.Expression)

type config = {
  prog : Prog.t;
  code : outcome;
  state : state;
  pc : pc;
  solver : Encoding.Batch.t;
  opt : Encoding.Optimizer.t;
}

and outcome =
  | Cont of Stmt.t list
  | Error of Expr.t option
  | Final of Expr.t option
  | Failure of string * Expr.t option
  | Unknown of Expr.t option

and func = string
and stack = Sstore.t Call_stack.t
and state = Expr.t S_heap.t * Sstore.t * stack * func
and pc = ESet.t

let is_cont (o : outcome) : bool = match o with Cont _ -> true | _ -> false
let is_fail (o : outcome) : bool = match o with Failure _ -> true | _ -> false
let is_final (o : outcome) : bool = match o with Final _ -> true | _ -> false
let update (c : config) code state pc : config = { c with code; state; pc }

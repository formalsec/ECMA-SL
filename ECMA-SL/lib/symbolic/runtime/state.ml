type config = {
  prog : Prog.t;
  code : outcome;
  state : state;
  pc : pc;
  solver : Z3.Solver.solver;
}

and outcome =
  | Cont of Stmt.t list
  | Error of Sval.t option
  | Final of Sval.t option
  | Failure of Sval.t option
  | Unknown of Sval.t option

and func = string
and stack = Sstore.t Call_stack.t
and state = Sval.t Heap.t * Sstore.t * stack * func
and pc = Sval.t list

let is_fail (o : outcome) : bool = match o with Failure _ -> true | _ -> false
let is_final (o : outcome) : bool = match o with Final _ -> true | _ -> false
let update (c : config) code state pc : config = { c with code; state; pc }

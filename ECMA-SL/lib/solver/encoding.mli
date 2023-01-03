exception Unknown

val time_solver : float ref
val mk_solver : unit -> Z3.Solver.solver
val clone : Z3.Solver.solver -> Z3.Solver.solver
val add : Z3.Solver.solver -> Sval.t list -> unit
val pop : Z3.Solver.solver -> int -> unit
val push : Z3.Solver.solver -> unit
val check : Z3.Solver.solver -> Sval.t list -> bool

val model :
  Z3.Solver.solver ->
  (Z3.Sort.sort * Z3.Symbol.symbol * Z3.Expr.expr option) list

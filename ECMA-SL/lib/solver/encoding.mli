exception Unknown
exception Error of string

val time_solver : float ref
val encode_value : Sval.t -> Z3.Expr.expr
val mk_solver : unit -> Z3.Solver.solver
val mk_opt : unit -> Z3.Optimize.optimize
val clone : Z3.Solver.solver -> Z3.Solver.solver
val add : Z3.Solver.solver -> Sval.t list -> unit
val pop : Z3.Solver.solver -> int -> unit
val push : Z3.Solver.solver -> unit
val check : Z3.Solver.solver -> Sval.t list -> bool
val maximize : Z3.Optimize.optimize -> Sval.t -> Sval.t list -> int
val minimize : Z3.Optimize.optimize -> Sval.t -> Sval.t list -> int
val optimize : Z3.Optimize.optimize -> Sval.t -> Sval.t list -> (Z3.Optimize.optimize -> Z3.Expr.expr -> Z3.Optimize.handle) -> int
val get_const_interp : Z3.Solver.solver -> Sval.t -> Sval.t list -> int

val model :
  Z3.Solver.solver ->
  Sval.t list ->
  (Z3.Sort.sort * Z3.Symbol.symbol * Z3.Expr.expr option) list

val string_of_value : Z3.Expr.expr -> string

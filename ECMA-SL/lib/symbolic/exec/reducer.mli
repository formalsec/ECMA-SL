val reduce_unop : Operators.uopt -> Expr.t -> Expr.t
val reduce_binop : Operators.bopt -> Expr.t -> Expr.t -> Expr.t
val reduce_triop : Operators.topt -> Expr.t -> Expr.t -> Expr.t -> Expr.t
val reduce_nop : Operators.nopt -> Expr.t list -> Expr.t

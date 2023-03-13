type t = Sval.t

val eval_unop : Operators.uopt -> t -> t
val eval_binop : Operators.bopt -> t -> t -> t
val eval_triop : Operators.topt -> t -> t -> t -> t
val eval_nop : Operators.nopt -> t list -> t

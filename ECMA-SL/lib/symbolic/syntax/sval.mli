type t =
  | Void
  | Null
  | Flt of float
  | Int of int
  | Bool of bool
  | Str of string
  | Loc of Loc.t
  | List of t list
  | Arr of t array
  | Type of Type.t
  | Tuple of t list
  | Symbol of string
  | Curry of string * t list
  | Byte of int
  | Symbolic of Type.t * string
  | Unop of Operators.uopt * t
  | Binop of Operators.bopt * t * t

val of_val : Val.t -> t
val is_symbol : t -> bool
val is_symbolic : t -> bool
val str : ?flt_with_dot:bool -> t -> string
val to_int : t -> int
val to_float : t -> float
val to_list : t -> t list
val to_tuple : t -> t list

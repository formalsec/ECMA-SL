type t =
  | IsSymbolic of string * Expr.t
  | IsSat of string * Expr.t
  | Maximize of string * Expr.t
  | Minimize of string * Expr.t
  | Eval of string * Expr.t

let str ?(print_expr : (Expr.t -> string) option) (st : t) : string =
  let str_e = Option.default Expr.str print_expr in
  match st with
  | IsSymbolic (s, e) -> "is_symbolic( " ^ str_e e ^ ")"
  | IsSat (s, e) -> "is_sat( " ^ str_e e ^ ")"
  | Maximize (s, e) -> "maximize( " ^ str_e e ^ ")"
  | Minimize (s, e) -> "minimize( " ^ str_e e ^ ")"
  | Eval (s, e) -> "eval( " ^ str_e e ^ ")"

type t =
  | IsSymbolic of string * Expr.t
  | IsSat of string * Expr.t
  | Maximize of string * Expr.t
  | Minimize of string * Expr.t
  | Eval of string * Expr.t
  | IsNumber of string * Expr.t

let str ?(print_expr : (Expr.t -> string) option) (st : t) : string =
  let str_e = Option.default Expr.str print_expr in
  match st with
  | IsSymbolic (s, e) -> s ^ " := __api_is_symbolic(" ^ str_e e ^ ")"
  | IsNumber (s, e) -> s ^ " := __api_is_number(" ^ str_e e ^ ")"
  | IsSat (s, e) -> s ^ " := __api_is_sat(" ^ str_e e ^ ")"
  | Maximize (s, e) -> s ^ " := __api_maximize(" ^ str_e e ^ ")"
  | Minimize (s, e) -> s ^ " := __api_minimize(" ^ str_e e ^ ")"
  | Eval (s, e) -> s ^ " := __api_eval(" ^ str_e e ^ ")"
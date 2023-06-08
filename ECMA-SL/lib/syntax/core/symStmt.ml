open Core

type t =
  | Assume of Expr.t
  | Evaluate of string * Expr.t
  | Maximize of string * Expr.t
  | Minimize of string * Expr.t
  | Is_symbolic of string * Expr.t
  | Is_sat of string * Expr.t
  | Is_number of string * Expr.t

let str ?(print_expr : (Expr.t -> string) option) (op : t) : string =
  let str_e = Option.value ~default:Expr.str print_expr in
  match op with
  | Assume e -> sprintf "se_assume(%s)" (str_e e)
  | Evaluate (x, e) -> sprintf "%s := se_evaluate(%s)" x (str_e e)
  | Maximize (x, e) -> sprintf "%s := se_maximize(%s)" x (str_e e)
  | Minimize (x, e) -> sprintf "%s := se_minimize(%s)" x (str_e e)
  | Is_symbolic (x, e) -> sprintf "%s := se_is_symbolic(%s)" x (str_e e)
  | Is_sat (x, e) -> sprintf "%s := se_is_sat(%s)" x (str_e e)
  | Is_number (x, e) -> sprintf "%s := se_is_number(%s)" x (str_e e)

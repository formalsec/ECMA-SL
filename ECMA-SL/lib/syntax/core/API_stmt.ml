open Core

type t =
  | Is_symbolic of string * Expr.t
  | Is_sat of string * Expr.t
  | Maximize of string * Expr.t
  | Minimize of string * Expr.t
  | Eval of string * Expr.t
  | Eval_wrapper of string * Expr.t
  | Exec_wrapper of string * Expr.t

let str ?(print_expr : (Expr.t -> string) option) (op : t) : string =
  let str_e = Option.value ~default:Expr.str print_expr in
  match op with
  | Is_symbolic (s, e) -> sprintf "%s := __api_is_symbolic(%s)" s (str_e e)
  | Is_sat (s, e) -> sprintf "%s := __api_is_sat(%s)" s (str_e e)
  | Maximize (s, e) -> sprintf "%s := __api_maximize(%s)" s (str_e e)
  | Minimize (s, e) -> sprintf "%s := __api_minimize(%s)" s (str_e e)
  | Eval (s, e) -> sprintf "%s := __api_eval(%s)" s (str_e e)
  | Eval_wrapper (s, e) -> sprintf "%s := __api_eval_wrapper(%s)" s (str_e e)
  | Exec_wrapper (s, e) -> sprintf "%s := __api_exec_wrapper(%s)" s (str_e e)

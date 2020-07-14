open Expr

type t = Skip
       | Assign      of string * Expr.t
       | Seq         of t * t
       | If          of (Expr.t option * t) list
       | While       of Expr.t * t
       | Return      of Expr.t
       | FieldAssign of Expr.t * Expr.t * Expr.t
       | FieldDelete of Expr.t * Expr.t
       | ExprStmt    of Expr.t

let rec str (stmt : t) : string = match stmt with
    Skip                      -> ""
  | Assign (v, exp)           -> v ^ " = " ^ (Expr.str exp)
  | Seq (s1, s2)              -> (str s1) ^ "; " ^ (str s2)
  | If (exps_stmts)           -> List.fold_left (fun strng e_s -> (if strng <> "" then strng ^ " else " else strng) ^ build_ifelse e_s) "" exps_stmts
  | While (exp, s)            -> "while (" ^ (Expr.str exp) ^ ") { " ^ (str s) ^ " }"
  | Return exp                -> "return " ^ (Expr.str exp) ^ ";"
  | FieldAssign (e_o, f, e_v) -> Expr.str e_o ^ "[" ^ Expr.str f ^ "] = " ^ Expr.str e_v
  | FieldDelete (e, f)        -> "delete " ^ Expr.str e ^ "[" ^ Expr.str f ^ "]"
  | ExprStmt e                -> Expr.str e

and build_ifelse (exp_stmt : Expr.t option * t) : string =
  match exp_stmt with
  | Some e, s -> "if (" ^ (Expr.str e) ^ ") { " ^ (str s) ^ " }"
  | None, s   -> "{ " ^ (str s) ^ " }"

type t = Skip
       | Assign      of string * E_Expr.t
       | Seq         of t * t
       | If          of (E_Expr.t option * t) list
       | While       of E_Expr.t * t
       | Return      of E_Expr.t
       | FieldAssign of E_Expr.t * E_Expr.t * E_Expr.t
       | FieldDelete of E_Expr.t * E_Expr.t
       | ExprStmt    of E_Expr.t
       | RepeatUntil of t * E_Expr.t
       | MatchWith   of E_Expr.t * (E_Expr.t * t) list

let rec str (stmt : t) : string = match stmt with
    Skip                      -> ""
  | Assign (v, exp)           -> v ^ " = " ^ (E_Expr.str exp)
  | Seq (s1, s2)              -> (str s1) ^ "; " ^ (str s2)
  | If (exps_stmts)           -> List.fold_left (fun strng e_s -> (if strng <> "" then strng ^ " else " else strng) ^ build_ifelse e_s) "" exps_stmts
  | While (exp, s)            -> "while (" ^ (E_Expr.str exp) ^ ") { " ^ (str s) ^ " }"
  | Return exp                -> "return " ^ (E_Expr.str exp) ^ ";"
  | FieldAssign (e_o, f, e_v) -> E_Expr.str e_o ^ "[" ^ E_Expr.str f ^ "] = " ^ E_Expr.str e_v
  | FieldDelete (e, f)        -> "delete " ^ E_Expr.str e ^ "[" ^ E_Expr.str f ^ "]"
  | ExprStmt e                -> E_Expr.str e
  | RepeatUntil (s, e)        -> "repeat " ^ str s ^ " until " ^ E_Expr.str e
  | MatchWith (e, exps_stmts) -> "match " ^ E_Expr.str e ^ " with | " ^ String.concat " | " (List.map (fun e_s -> E_Expr.str (fst e_s) ^ ": " ^ str (snd e_s)) exps_stmts)

and build_ifelse (exp_stmt : E_Expr.t option * t) : string =
  match exp_stmt with
  | Some e, s -> "if (" ^ (E_Expr.str e) ^ ") { " ^ (str s) ^ " }"
  | None, s   -> "{ " ^ (str s) ^ " }"

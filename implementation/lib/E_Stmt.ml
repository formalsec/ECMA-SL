type t = Skip
       | Assign      of string * E_Expr.t
       | Block       of t list
       | If          of E_Expr.t * t * t option
       | While       of E_Expr.t * t
       | Return      of E_Expr.t
       | FieldAssign of E_Expr.t * E_Expr.t * E_Expr.t
       | FieldDelete of E_Expr.t * E_Expr.t
       | ExprStmt    of E_Expr.t
       | RepeatUntil of t * E_Expr.t
       | MatchWith   of E_Expr.t * (E_Pat.t * t) list

let rec str (stmt : t) : string = match stmt with
    Skip                      -> ""
  | Assign (v, exp)           -> v ^ " = " ^ (E_Expr.str exp)
  | Block stmts               -> "{ " ^ String.concat ";" (List.map str stmts) ^ " }"
  | If (e, s1, s2)            -> (let v = "if (" ^ E_Expr.str e ^ ") " ^ str s1 in
                                  match s2 with
                                  | None   -> v
                                  | Some s -> v ^ " else " ^ str s)
  | While (exp, s)            -> "while (" ^ (E_Expr.str exp) ^ ") " ^ (str s)
  | Return exp                -> "return " ^ (E_Expr.str exp) ^ ";"
  | FieldAssign (e_o, f, e_v) -> E_Expr.str e_o ^ "[" ^ E_Expr.str f ^ "] = " ^ E_Expr.str e_v
  | FieldDelete (e, f)        -> "delete " ^ E_Expr.str e ^ "[" ^ E_Expr.str f ^ "]"
  | ExprStmt e                -> E_Expr.str e
  | RepeatUntil (s, e)        -> "repeat " ^ str s ^ " until " ^ E_Expr.str e
  | MatchWith (e, pats_stmts) -> "match " ^ E_Expr.str e ^ " with | "
                                 ^ String.concat " | " (List.map (fun (e, s) -> E_Pat.str e ^ ": " ^ str s) pats_stmts)

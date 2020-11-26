type t = Skip
       | Print       of E_Expr.t
       | Assign      of string * E_Expr.t
       | GlobAssign  of string * E_Expr.t 
       | Block       of t list
       | If          of E_Expr.t * t * t option
       | While       of E_Expr.t * t
       | Return      of E_Expr.t
       | FieldAssign of E_Expr.t * E_Expr.t * E_Expr.t
       | FieldDelete of E_Expr.t * E_Expr.t
       | ExprStmt    of E_Expr.t
       | RepeatUntil of t * E_Expr.t
       | MatchWith   of E_Expr.t * (E_Pat.t * t) list
       | Throw       of E_Expr.t
       | Assert      of E_Expr.t
       | MacroApply  of string * E_Expr.t list
       | Switch      of E_Expr.t * (E_Expr.t * t) list * t option

(*
switch (e) {
  case x: 3 
  case y: 33
  default: 333
}
*)

let rec str (stmt : t) : string = 
  let str_cases cases = 
    let strs = 
      List.map 
        (fun (e, s) -> Printf.sprintf "case %s: %s" (E_Expr.str e) (str s)) 
        cases in 
    String.concat "\n" strs in 

  let str_o = 
    Option.map_default  
      (fun s -> Printf.sprintf "default: %s" (str s))
      "" in   

  match stmt with
      Skip                      -> ""
    | Print e                   -> "print " ^ (E_Expr.str e)
    | Assign (x, exp)           -> x ^ " := " ^ (E_Expr.str exp)
    | GlobAssign (x, exp)       -> "|" ^ x ^ "| := " ^ (E_Expr.str exp)
    | Block stmts               -> "{ " ^ String.concat ";" (List.map str stmts) ^ " }"
    | If (e, s1, s2)            -> (let v = "if (" ^ E_Expr.str e ^ ") " ^ str s1 in
                                    match s2 with
                                    | None   -> v
                                    | Some s -> v ^ " else " ^ str s)
    | While (exp, s)            -> "while (" ^ (E_Expr.str exp) ^ ") " ^ (str s)
    | Return exp                -> "return " ^ (E_Expr.str exp)
    | FieldAssign (e_o, f, e_v) -> E_Expr.str e_o ^ "[" ^ E_Expr.str f ^ "] := " ^ E_Expr.str e_v
    | FieldDelete (e, f)        -> "delete " ^ E_Expr.str e ^ "[" ^ E_Expr.str f ^ "]"
    | ExprStmt e                -> E_Expr.str e
    | RepeatUntil (s, e)        -> "repeat " ^ str s ^ " until " ^ E_Expr.str e
    | MatchWith (e, pats_stmts) -> "match " ^ E_Expr.str e ^ " with | "
                                  ^ String.concat " | " (List.map (fun (e, s) -> E_Pat.str e ^ ": " ^ str s) pats_stmts)
    | Throw e                   -> "throw " ^ E_Expr.str e
    | Assert e                  -> "assert " ^ E_Expr.str e
    | MacroApply (m, es)        -> "@" ^ m ^ " (" ^ String.concat ", " (List.map E_Expr.str es) ^ ")"
    | Switch (e, cases, so)     -> Printf.sprintf "switch (%s) { %s %s }" (E_Expr.str e) (str_cases cases) (str_o so)


let rec map
      ?(fe : (E_Expr.t -> E_Expr.t) option) 
      (f : (t -> t)) (s : t) : t = 
  
  let fe = Option.default (fun x -> x) fe in 
  let f_pat = List.map (fun (epat, s) -> (epat, map ~fe f s)) in 
  let f_cases =  List.map (fun (e, s) -> (fe e, map ~fe f s)) in 

  let fx (x : string) : string = 
    let e' = fe (E_Expr.Var x) in 
    match (e' : E_Expr.t) with 
      | Var y -> y 
      | _ -> raise (Failure "Substituting non-var expression on LHS") in 
 
  let s' = 
    match s with 
    | Skip                        -> Skip 
    | Print e                     -> Print (fe e)
    | Assign (x, e)               -> Assign (fx x, fe e)
    | GlobAssign (x, e)           -> GlobAssign (fx x, fe e)
    | Block ss                    -> Block (List.map (map ~fe f) ss)
    | If (e, s1, s2)              -> If (fe e, (map ~fe f s1), Option.map (map ~fe f) s2)
    | While (e, s)                -> While(fe e, map ~fe f s)
    | Return e                    -> Return (fe e)
    | FieldAssign (e_o, e_f, e_v) -> FieldAssign (fe e_o, fe e_f, fe e_v)
    | FieldDelete (e, f)          -> FieldDelete (fe e, fe f) 
    | ExprStmt e                  -> ExprStmt (fe e)
    | RepeatUntil (s, e)          -> RepeatUntil (map ~fe f s, fe e)
    | MatchWith (e, pats_stmts)   -> MatchWith (fe e, f_pat pats_stmts)
    | Throw e                     -> Throw (fe e)
    | Assert e                    -> Assert (fe e)
    | MacroApply (m, es)          -> MacroApply (m, List.map fe es)  
    | Switch (e, cases, so)       -> Switch (fe e, f_cases cases, Option.map (map ~fe f) so) in 
  f s' 

let subst (sbst : E_Expr.subst_t) (s : t) : t = 
  (*Printf.printf "Applying the subst: %s\nOn statement:\n%s\n" (E_Expr.string_of_subst sbst) (str s); *)
  let ret = map ~fe:(E_Expr.subst sbst) (fun x -> x) s in 
  (* Printf.printf "Obtained: %s\n" (str ret);  *)
  ret 

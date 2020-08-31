type t = Skip
       | Print        of Expr.t
       | Assign       of string * Expr.t
       | If           of Expr.t * t * t option
       | While        of Expr.t * t
       | Return       of Expr.t
       | AssignCall   of string * Expr.t * Expr.t list
       | AssignNewObj of string
       | AssignInObjCheck of string * Expr.t * Expr.t
       | Block of t list
       | FieldAssign  of Expr.t * Expr.t * Expr.t
       | FieldDelete  of Expr.t * Expr.t
       | FieldLookup of string * Expr.t * Expr.t

(*---------------Strings------------------*)

let rec str (stmt : t) : string = match stmt with
    Skip                        -> ""
  | Print e                     -> "print " ^ (Expr.str e)
  | Assign (v, exp)             -> v ^ " := " ^ (Expr.str exp)
  | If (e, s1, s2)              -> (let v = "if (" ^ Expr.str e ^ ") {\n" ^ str s1 ^ "\n}" in
                                    match s2 with
                                    | None   -> v
                                    | Some s -> v ^ " else {\n" ^ str s ^ "\n}" )
  | Block (block)               -> String.concat ";\n" (List.map str block)
  | While (exp, s)              -> "while (" ^ (Expr.str exp) ^ ") { " ^ (str s) ^ " }"
  | Return exp                  -> "return " ^ (Expr.str exp)
  | FieldAssign (e_o, f, e_v)   -> Expr.str e_o ^ "[" ^ Expr.str f ^ "] := " ^ Expr.str e_v
  | FieldDelete (e, f)          -> "delete " ^ Expr.str e ^ "[" ^ Expr.str f ^ "]"
  | AssignCall (va, st, e_lst)  -> va ^ " := " ^ Expr.str st ^ " (" ^ String.concat ", " (List.map (fun e -> Expr.str e) e_lst) ^ ")"
  | AssignNewObj va             -> va ^ " := { }"
  | FieldLookup (va, eo, p)    -> va ^ " := " ^ Expr.str eo ^ "[" ^ Expr.str p ^ "]"
  | AssignInObjCheck (st,e1,e2) -> st ^ " := " ^ Expr.str e1 ^ " in_obj " ^ Expr.str e2

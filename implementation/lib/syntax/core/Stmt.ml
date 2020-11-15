type t = Skip
       | Merge
       | Print            of Expr.t
       | Assign           of string * Expr.t
       | If               of Expr.t * t * t option
       | While            of Expr.t * t
       | Return           of Expr.t
       | AssignCall       of string * Expr.t * Expr.t list
       | AssignNewObj     of string
       | AssignInObjCheck of string * Expr.t * Expr.t
       | AssignObjToList  of string * Expr.t
       | AssignObjFields  of string * Expr.t
       | Block            of t list
       | FieldAssign      of Expr.t * Expr.t * Expr.t
       | FieldDelete      of Expr.t * Expr.t
       | FieldLookup      of string * Expr.t * Expr.t
       | Exception        of string         
(*---------------Strings------------------*)

let rec str (stmt : t) : string = match stmt with
    Skip
  | Merge                       -> ""
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
  | FieldLookup (va, eo, p)     -> va ^ " := " ^ Expr.str eo ^ "[" ^ Expr.str p ^ "]"
  | AssignInObjCheck (st,e1,e2) -> st ^ " := " ^ Expr.str e1 ^ " in_obj " ^ Expr.str e2
  | AssignObjToList (st, e)     -> st ^ " := obj_to_list " ^ Expr.str e
  | AssignObjFields (st, e)     -> st ^ " := obj_fields " ^ Expr.str e
  | Exception st                 -> "exception( " ^ st ^ " )"

let rec js (stmt : t) : string = 
match stmt with
| Skip                            -> Printf.sprintf ""
| Merge                           -> Printf.sprintf ""
| Assign (x, e)                   -> Printf.sprint "%s = %s" x (Expr.js e)
| Print e                         -> Printf.sprintf "console.log( %s )" (Expr.js e)
| If (e, s1, s2)                  -> Printf.sprintf "if ( %s ) { %s } %s" (Expr.js e) (js s1)
                                      (match s2 with
                                      | None   -> Printf.sprintf ""
                                      | Some s -> Printf.sprintf " else { %s }" (js s))
| Block (block)                   -> String.concat ";\n" (List.map js block)
| While (exp, s)                  -> Printf.sprintf "while ( %s ) { %s } " (Expr.js exp) (js s)
| Return exp                      -> Printf.sprintf "return %s" (Expr.js exp)
| FieldAssign (e_o, e_f, e_v)     -> Printf.sprintf "%s.%s = $s" (Expr.js e_o) (Expr.js e_f) (Expr.js e_v)
| FieldDelete (e_o, e_f)          -> Printf.sprintf "delete %s.%s" (Expr.js e_o) (Expr.js e_f)
| AssignCall (x, st, e_lst)       -> Printf.sprintf "%s = %s( %s )" x st (String.concat ", " (List.map Expr.js e_lst))
| AssignNewObj x                  -> Printf.sprintf "var %s = {}" x
| FieldLookup (x, e_o, e_f)       -> Printf.sprintf "%s = %s.%s" x (Expr.js e_o) (Expr.js e_f)
| AssignInObjCheck (x, e_o, e_f)  -> Printf.sprintf "%s = %s.hasOwnProperty( %s )" x (Expr.js e_o) (Expr.js e_f)
| AssignObjToList (x, e)          -> Printf.sprintf "%s = Object.values(%s)" x (Expr.js e)
| AssignObjFields (x, e)          -> Printf.sprintf "%s = Object.keys(%s)" x (Expr.js e)
| Exception st                    -> Printf.sprintf "throw %s" st
  
let rec to_json (stmt : t) : string =
  (*Stmts args : rhs/ lhs / expr / obj / field/ stringvar *)
  match stmt with
  | Skip                        -> Printf.sprintf "{\"type\" : \"skip\"}"
  | Merge                       -> Printf.sprintf "{\"type\" : \"merge\"}"
  | Print e                     -> Printf.sprintf "{\"type\" : \"print\", \"expr\" :  %s }" (Expr.to_json e)
  | Assign (v, exp)             -> Printf.sprintf "{\"type\" : \"assign\", \"lhs\" :  \"%s\", \"rhs\" :  %s}" v (Expr.to_json exp)
  | If (e, s1, s2)              -> Printf.sprintf "{\"type\" : \"condition\", \"expr\" : %s, \"then\" : %s %s}" (Expr.to_json e) (to_json s1) (match s2 with
                                                                                                                                        | Some v -> Printf.sprintf ",\"else\" : %s" (to_json v)
                                                                                                                                        | None -> "")
  | Block (block)               -> Printf.sprintf "{\"type\" : \"block\", \"value\" : [ %s ]}" (String.concat ", " (List.map to_json block))
  | While (exp, s)              -> Printf.sprintf "{\"type\" : \"loop\", \"expr\" : %s, \"do\" : %s }" (Expr.to_json exp) (to_json s)
  | Return exp                  -> Printf.sprintf "{\"type\" : \"return\", \"expr\" : %s}" (Expr.to_json exp)
  | FieldAssign (e_o, f, e_v)   -> Printf.sprintf "{\"type\" : \"fieldassign\", \"obj\" : %s, \"field\" : %s, \"value\" : %s}" (Expr.to_json e_o) (Expr.to_json f) (Expr.to_json e_v)
  | FieldDelete (e, f)          -> Printf.sprintf "{\"type\" : \"fielddelete\", \"obj\" : %s, \"field\" : %s}" (Expr.to_json e) (Expr.to_json f)
  | AssignCall (va, st, e_lst)  -> Printf.sprintf "{\"type\" : \"assigncall\", \"lhs\" : \"%s\", \"func\" : %s, \"args\" : [%s]}" (va) (Expr.to_json st) (String.concat ", " (List.map Expr.to_json e_lst))
  | AssignNewObj va             -> Printf.sprintf "{\"type\" : \"assignnewobject\", \"lhs\" : \"%s\" }" (va)
  | FieldLookup (va, eo, p)     -> Printf.sprintf "{\"type\" : \"fieldlookup\", \"lhs\" : \"%s\", \"obj\" : %s, \"field\" : %s}" (va) (Expr.to_json eo) (Expr.to_json p)
  | AssignInObjCheck (st,e1,e2) -> Printf.sprintf "{\"type\" : \"assigninobjcheck\", \"lhs\" : \"%s\", \"field\" : %s, \"obj\" : %s}" (st) (Expr.to_json e1) (Expr.to_json e2)
  | AssignObjToList (st,e)      -> Printf.sprintf "{\"type\" : \"assigniobjtolist\", \"lhs\" : \"%s\", \"obj\" %s}" (st) (Expr.to_json e) 
  | AssignObjFields (st,e)      -> Printf.sprintf "{\"type\" : \"assignobjfields\", \"lhs\" : \"%s\", \"obj\" %s}" (st) (Expr.to_json e)
  | Exception st                -> Printf.sprintf "{\"type\" : \"exception\", \"value\" : %s}" (st)
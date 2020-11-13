type t = Val    of Val.t
       | Var    of string
       | BinOpt of (Oper.bopt * t * t)
       | UnOpt  of (Oper.uopt * t)
       | NOpt   of Oper.nopt * t list

(*-----------String-------------*)

let rec str (e : t) : string = 
  match e with
  | Val n               -> Val.str n
  | Var x               -> x
  | UnOpt (op, e)       -> (Oper.str_of_unopt op) ^ "(" ^ (str e) ^ ")"
  | BinOpt (op, e1, e2) -> (Oper.str_of_binopt op (str e1) (str e2))
  | NOpt (op, es)       -> (Oper.str_of_nopt op (List.map str es))




let rec vars (exp:t): string list =
  (*returns every var used in exp*)
  match exp with
  | Var x             -> [x]
  | UnOpt (op,e)      -> (vars e)
  | BinOpt (op,e1,e2) -> (vars e1) @ (vars e2)
  | NOpt (op,es)      ->  List.concat (List.map vars es)
  | _                 -> []

let rec to_json (e : t): string =
  match e with
  | Val v               -> Printf.sprintf "{ \"type\" : \"value\", \"value\" : %s }" (Val.to_json v)
  | Var x               -> Printf.sprintf "{ \"type\" : \"var\", \"name\" : \"%s\"}" x
  | UnOpt (op, e)       -> Printf.sprintf "{ \"type\" : \"unop\", \"rhs\" : %s, \"op\": %s}" (to_json e) (Oper.uopt_to_json op)  
  | BinOpt (op, e1, e2) -> Printf.sprintf "{ \"type\" : \"binop\", \"lhs\" : %s, \"rhs\": %s,  \"op\": %s}" (to_json e1) (to_json e2) (Oper.bopt_to_json op) 
  | NOpt (op, es)       -> Printf.sprintf "{ \"type\" : \"nop\", \"op\": %s, \"args\" : [ %s ]}"  (Oper.nopt_to_json op) (String.concat ", " (List.map to_json es))  

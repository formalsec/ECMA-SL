type t = Val    of Val.t
       | Var    of string
       | BinOpt of (Oper.bopt * t * t)
       | UnOpt  of (Oper.uopt * t)
       | NOpt   of Oper.nopt * t list

(*-----------String-------------*)

let rec str (e : t) : string = match e with
  | Val n               -> Val.str n
  | Var x               -> x
  | UnOpt (op, e)       -> (Oper.str_of_unopt op) ^ "(" ^ (str e) ^ ")"
  | BinOpt (op, e1, e2) -> (str e1) ^ " " ^ (Oper.str_of_binopt op) ^ " " ^ (str e2)
  | NOpt (op, es)       -> (Oper.str_of_nopt op (List.map str es))

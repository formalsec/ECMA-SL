type bopt = Plus
          | Minus
          | Times
          | Div
          | Equal
          | Gt
          | Lt
          | Egt
          | Elt
          | Log_And
          | Log_Or
          | InObj

type uopt = Neg
          | Not
          | Typeof

type nopt = ListExpr

type t = Val    of Val.t
       | Var    of string
       | BinOpt of (bopt * t * t)
       | UnOpt  of (uopt * t)
       | NOpt   of nopt * t list

(*-----------String-------------*)

let str_of_unopt (op : uopt) : string = match op with
  | Neg    -> "-"
  | Not    -> "!"
  | Typeof -> "typeof"

let str_of_binopt (op : bopt) : string = match op with
  | Plus    -> "+"
  | Minus   -> "-"
  | Times   -> "*"
  | Div     -> "/"
  | Equal   -> "=="
  | Gt      -> ">"
  | Lt      -> "<"
  | Egt     -> ">="
  | Elt     -> "<="
  | Log_And -> "&&"
  | Log_Or  -> "||"
  | InObj   -> "in"

let rec str (e : t) : string = match e with
  | Val n               -> Val.str n
  | Var x               -> x
  | UnOpt (op, e)       -> (str_of_unopt op) ^ "(" ^ (str e) ^ ")"
  | BinOpt (op, e1, e2) -> (str e1) ^ " " ^ (str_of_binopt op) ^ " " ^ (str e2)
  | NOpt (op, es)       -> (str_of_nopt op es)

and str_of_nopt (op : nopt) (es : t list) : string = match op with
  | ListExpr -> "[ " ^ List.fold_left (fun acc ele -> (if acc <> "" then acc ^ ", " else acc) ^ str ele) "" es ^ " ]"

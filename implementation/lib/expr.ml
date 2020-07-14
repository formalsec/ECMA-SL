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
       | Call   of (t * t list)
       | NewObj of (Field.t * t) list
       | Access of t * t

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
  | Call (f, es)        -> (str f) ^ " (" ^ List.fold_left (fun acc ele -> acc ^ str ele) "" es ^ ")"
  | NewObj (fes)        -> "{ " ^ fields_list_to_string fes ^ " }"
  | Access (e, f)       -> str e ^ "[" ^ str f ^ "]"

and str_of_nopt (op : nopt) (es : t list) : string = match op with
  | ListExpr -> "[ " ^ List.fold_left (fun acc ele -> (if acc <> "" then acc ^ ", " else acc) ^ str ele) "" es ^ " ]"

and fields_list_to_string (fes : (Field.t * t) list) : string =
  let strs = List.map (fun (f, e) -> (Field.str f) ^ ": " ^ (str e)) fes in
  String.concat ", " strs

type t =
  | Val    of Val.t
  | Var    of string
  | BinOpt of Oper.bopt * t * t
  | UnOpt  of Oper.uopt * t
  | NOpt   of Oper.nopt * t list
  | Call   of t * t list
  | NewObj of (string * t) list
  | Access of t * t


let rec str (e : t) : string = match e with
  | Val n               -> Val.str n
  | Var x               -> x
  | UnOpt (op, e)       -> (Oper.str_of_unopt op) ^ "(" ^ (str e) ^ ")"
  | BinOpt (op, e1, e2) -> (str e1) ^ " " ^ (Oper.str_of_binopt op) ^ " " ^ (str e2)
  | NOpt (op, es)       -> (Oper.str_of_nopt op (List.map str es))
  | Call (f, es)        -> (str f) ^ " (" ^ String.concat ", " (List.map str es) ^ ")"
  | NewObj (fes)        -> "{ " ^ fields_list_to_string fes ^ " }"
  | Access (e, f)       -> str e ^ "[" ^ str f ^ "]"


and fields_list_to_string (fes : (string * t) list) : string =
  let strs = List.map (fun (f, e) -> f ^ ": " ^ (str e)) fes in
  String.concat ", " strs

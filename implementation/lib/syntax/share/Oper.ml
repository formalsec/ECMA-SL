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
          | Lnth

type uopt = Neg
          | Not
          | Typeof
          | Len
          | Head
          | Tail

type nopt = ListExpr
          | TupleExpr
          | NAry_And
          | NAry_Or

let neg (v : Val.t) : Val.t = match v with
  | Flt v    -> Flt (-.v)
  | Int v    -> Int (-v)
  | _        -> invalid_arg "Exception in Oper.neg: this operation is only applicable to Float or Int arguments"

let not (v : Val.t) : Val.t = match v with
  | Bool v -> Bool (v = false)
  | _      -> invalid_arg "Exception in Oper.not: this operation is only applicable to a boolean type argument"

let plus (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 +. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 +. v2)
  | (Flt v1, Flt v2) -> Flt (v1 +. v2)
  | (Int v1, Int v2) -> Int (v1 + v2)
  | _                -> invalid_arg "Exception in Oper.plus: this operation is only applicable to Float or Int arguments"

let minus (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 -. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 -. v2)
  | (Flt v1, Flt v2) -> Flt (v1 -. v2)
  | (Int v1, Int v2) -> Int (v1 - v2)
  | _                -> invalid_arg "Exception in Oper.minus: this operation is only applicable to Float or Int arguments"

let times (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 *. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 *. v2)
  | (Flt v1, Flt v2) -> Flt (v1 *. v2)
  | (Int v1, Int v2) -> Int (v1 * v2)
  | _                -> invalid_arg "Exception in Oper.times: this operation is only applicable to Float or Int arguments"

let div (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 /. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 /. v2)
  | (Flt v1, Flt v2) -> Flt (v1 /. v2)
  | (Int v1, Int v2) -> Int (v1 / v2)
  | _                -> invalid_arg "Exception in Oper.div: this operation is only applicable to Float or Int arguments"

let equal (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 = v2)

let gt (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 > v2)

let lt (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 < v2)

let egt (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 >= v2)

let elt (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 <= v2)

let log_and (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Bool v1, Bool v2 -> Bool (v1 && v2)
  | _                -> invalid_arg "Exception in Oper.log_and: this operation is only applicable to Bool arguments"

let log_or (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Bool v1, Bool v2 -> Bool (v1 || v2)
  | _                -> invalid_arg "Exception in Oper.log_or: this operation is only applicable to Bool arguments"

let is_true (v : Val.t) : bool = match v with
  | Bool v -> v
  | _      -> invalid_arg "Exception in Oper.is_true: argument is not boolean"

let typeof (v : Val.t) : Val.t = match v with
  | Int _    -> Val.Str (Type.str (Type.IntType))
  | Flt _    -> Val.Str (Type.str (Type.FltType))
  | Bool _   -> Val.Str (Type.str (Type.BoolType))
  | Str _    -> Val.Str (Type.str (Type.StrType))
  | Loc _    -> Val.Str (Type.str (Type.LocType))
  | List _   -> Val.Str (Type.str (Type.ListType))
  | Type _   -> Val.Str (Type.str (Type.TypeType))
  | Tuple _  -> Val.Str (Type.str (Type.TupleType))
  | Null     -> Val.Str (Type.str (Type.NullType))
  | Symbol _ -> Val.Str (Type.str (Type.SymbolType))
  | _        -> invalid_arg ("Exception in Oper.typeof: invalid argument \"" ^ Val.str v ^ "\"")

let len (v : Val.t) : Val.t = match v with
  | List v  -> Val.Int (List.length v)
  | _       -> invalid_arg "Exception in Oper.len: this operation is only applicable to List arguments"

let list_nth (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | List l, Int i -> List.nth l i
  | _             -> invalid_arg "Exception in Oper.list_nth: this operation is only applicable to List and Int arguments"

let head (v : Val.t) : Val.t = match v with
  | Tuple t -> List.hd t
  | _       -> invalid_arg "Exception in Oper.head: this operation is only applicable to Tuple arguments"

let tail (v : Val.t) : Val.t = match v with
  | Tuple t -> Tuple (List.tl t)
  | _       -> invalid_arg "Exception in Oper.head: this operation is only applicable to Tuple arguments"

let str_of_unopt (op : uopt) : string = match op with
  | Neg    -> "-"
  | Not    -> "!"
  | Typeof -> "typeof"
  | Len    -> "len"
  | Head   -> "hd"
  | Tail   -> "tl"

let str_of_binopt (op : bopt) (e1 : string) (e2 : string) : string = match op with
  | Plus    -> e1 ^ " + " ^ e2
  | Minus   -> e1 ^ " - " ^ e2
  | Times   -> e1 ^ " * " ^ e2
  | Div     -> e1 ^ " / " ^ e2
  | Equal   -> e1 ^ " = " ^ e2
  | Gt      -> e1 ^ " > " ^ e2
  | Lt      -> e1 ^ " < " ^ e2
  | Egt     -> e1 ^ " >= " ^ e2
  | Elt     -> e1 ^ " <= " ^ e2
  | Log_And -> e1 ^ " && " ^ e2
  | Log_Or  -> e1 ^ " || " ^ e2
  | InObj   -> e1 ^ " in " ^ e2
  | Lnth    -> "l_nth(" ^ e1 ^ ", " ^ e2 ^ ")"

let str_of_nopt (op : nopt) (es : string list) : string = match op with
  | ListExpr  -> "[ " ^ (String.concat ", " es) ^ " ]"
  | TupleExpr -> "( " ^ (String.concat ", " es) ^ " )"
  | NAry_And  -> String.concat " && " es
  | NAry_Or   -> String.concat " || " es

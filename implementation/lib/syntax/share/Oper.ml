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
          | InList
          | Lnth
          | Tnth
          | Ladd

type uopt = Neg
          | Not
          | Typeof
          | ListLen
          | TupleLen
          | Head
          | Tail
          | First
          | Second

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

(* ECMAScript does not perform integer division. The operands and result of all
   division operations are double-precision floating-point numbers. *)
let div (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 /. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 /. v2)
  | (Flt v1, Flt v2) -> Flt (v1 /. v2)
  | (Int v1, Int v2) -> Flt (float_of_int v1 /. float_of_int v2)
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
  | Int _    -> Type (Type.IntType)
  | Flt _    -> Type (Type.FltType)
  | Bool _   -> Type (Type.BoolType)
  | Str _    -> Type (Type.StrType)
  | Loc _    -> Type (Type.LocType)
  | List _   -> Type (Type.ListType)
  | Type _   -> Type (Type.TypeType)
  | Tuple _  -> Type (Type.TupleType)
  | Null     -> Type (Type.NullType)
  | Symbol _ -> Type (Type.SymbolType)
  | Void     -> invalid_arg ("Exception in Oper.typeof: unexpected void value")

let l_len (v : Val.t) : Val.t = match v with
  | List l -> Val.Int (List.length l)
  | _      -> invalid_arg "Exception in Oper.l_len: this operation is only applicable to List arguments"

let t_len (v : Val.t) : Val.t = match v with
  | Tuple t -> Val.Int (List.length t)
  | _       -> invalid_arg "Exception in Oper.t_len: this operation is only applicable to Tuple arguments"

let list_nth (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | List l, Int i -> List.nth l i
  | _             -> invalid_arg "Exception in Oper.list_nth: this operation is only applicable to List and Int arguments"

let tuple_nth (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Tuple l, Int i -> List.nth l i
  | _              -> invalid_arg "Exception in Oper.tuple_nth: this operation is only applicable to Tuple and Int arguments"

let list_in (v1, v2 : Val.t * Val.t) : Val.t = match v2 with
  | List l -> Bool (List.mem v1 l)
  | _      -> invalid_arg "Exception in Oper.list_in: this operation is only applicable to List arguments"

let list_add (v1, v2 : Val.t * Val.t) : Val.t = match v1 with
  | List l -> Val.List (v2 :: l)
  | _      -> invalid_arg "Exception in Oper.list_add: this operation is only applicable to List arguments"

let head (v : Val.t) : Val.t = match v with
  | List l -> List.hd l
  | _      -> invalid_arg "Exception in Oper.head: this operation is only applicable to List arguments"

let tail (v : Val.t) : Val.t = match v with
  | List l -> List (List.tl l)
  | _      -> invalid_arg "Exception in Oper.tail: this operation is only applicable to List arguments"

let first (v : Val.t) : Val.t = match v with
  | Tuple t -> List.hd t
  | _       -> invalid_arg "Exception in Oper.first: this operation is only applicable to Tuple arguments"

let second (v : Val.t) : Val.t = match v with
  | Tuple t -> Tuple (List.tl t)
  | _       -> invalid_arg "Exception in Oper.second: this operation is only applicable to Tuple arguments"

let str_of_unopt (op : uopt) : string = match op with
  | Neg      -> "-"
  | Not      -> "!"
  | Typeof   -> "typeof"
  | ListLen  -> "l_len"
  | TupleLen -> "t_len"
  | Head     -> "hd"
  | Tail     -> "tl"
  | First    -> "fst"
  | Second   -> "snd"

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
  | InObj   -> e1 ^ " in_obj " ^ e2
  | InList  -> e1 ^ " in_list " ^ e2
  | Lnth    -> "l_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Tnth    -> "t_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Ladd    -> "l_add(" ^ e1 ^ ", " ^ e2 ^ ")"

let str_of_nopt (op : nopt) (es : string list) : string = match op with
  | ListExpr  -> "[ " ^ (String.concat ", " es) ^ " ]"
  | TupleExpr -> "( " ^ (String.concat ", " es) ^ " )"
  | NAry_And  -> String.concat " && " es
  | NAry_Or   -> String.concat " || " es

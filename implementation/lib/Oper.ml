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

let neg (v : Val.t) : Val.t = match v with
  | Flt v  -> Flt (-.v)
  | Int v  -> Int (-v)
  | Bool v -> invalid_arg "Exception in Val.neg: this operation doesn't apply to boolean type argument"
  | Str v  -> invalid_arg "Exception in Val.neg: this operation doesn't apply to string type argument"
  | Loc v  -> invalid_arg "Exception in Val.neg: this operation doesn't apply to Loc type argument"
  | List v -> invalid_arg "Exception in Val.neg: this operation doesn't apply to List type argument"
  | Type v -> invalid_arg "Exception in Val.neg: this operation doesn't apply to Type type argument"
  | Void   -> invalid_arg "Exception in Val.neg: this operation doesn't apply to void type argument"
  | Undef  -> invalid_arg "Exception in Val.neg: this operation doesn't apply to undefined type argument"
  | Null   -> invalid_arg "Exception in Val.neg: this operation doesn't apply to null type argument"

let not (v : Val.t) : Val.t = match v with
  | Bool v -> Bool (v = false)
  | _      -> invalid_arg "Exception in Val.not: this operation is only applicable to a boolean type argument"

let plus (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 +. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 +. v2)
  | (Flt v1, Flt v2) -> Flt (v1 +. v2)
  | (Int v1, Int v2) -> Int (v1 + v2)
  | _                -> invalid_arg "Exception in Val.plus: this operation is only applicable to Float or Int arguments"

let minus (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 -. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 -. v2)
  | (Flt v1, Flt v2) -> Flt (v1 -. v2)
  | (Int v1, Int v2) -> Int (v1 - v2)
  | _                -> invalid_arg "Exception in Val.minus: this operation is only applicable to Float or Int arguments"

let times (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 *. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 *. v2)
  | (Flt v1, Flt v2) -> Flt (v1 *. v2)
  | (Int v1, Int v2) -> Int (v1 * v2)
  | _                -> invalid_arg "Exception in Val.times: this operation is only applicable to Float or Int arguments"

let div (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 /. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 /. v2)
  | (Flt v1, Flt v2) -> Flt (v1 /. v2)
  | (Int v1, Int v2) -> Int (v1 / v2)
  | _                -> invalid_arg "Exception in Val.div: this operation is only applicable to Float or Int arguments"

let equal (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 = v2)

let gt (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 > v2)

let lt (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 < v2)

let egt (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 >= v2)

let elt (v1, v2 : Val.t * Val.t) : Val.t = Bool (v1 <= v2)

let log_and (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Bool v1, Bool v2 -> Bool (v1 && v2)
  | _                -> invalid_arg "Exception in Val.log_and: this operation is only applicable to Bool arguments"

let log_or (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Bool v1, Bool v2 -> Bool (v1 || v2)
  | _                -> invalid_arg "Exception in Val.log_or: this operation is only applicable to Bool arguments"

let is_true (v : Val.t) : bool = match v with
  | Bool v -> v
  | _      -> invalid_arg "Exception in Val.is_true: argument is not boolean"

let typeof (v : Val.t) : Val.t = match v with
  | Int v  -> Type (Type.IntType)
  | Flt v  -> Type (Type.FltType)
  | Str v  -> Type (Type.StrType)
  | Bool v -> Type (Type.BoolType)
  | Loc v  -> invalid_arg "Exception in Val.typeof: not implemented for Loc type argument"
  | Type v -> invalid_arg "Exception in Val.typeof: not implemented for Type type argument"
  | _      -> invalid_arg "Exception in Val.typeof: invalid argument"

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

    let str_of_nopt (op : nopt) (es : string list) : string = match op with
      | ListExpr -> "[ " ^ List.fold_left (fun acc ele -> (if acc <> "" then acc ^ ", " else acc) ^ ele) "" es ^ " ]"

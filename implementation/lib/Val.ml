type t =
  | Flt   of float
  | Int   of int
  | Bool  of bool
  | Str   of string
  | Loc   of Loc.t
  | List  of t list
  | Type  of Type.t
  | Void
  | Undef
  | Null
  | Symbol of string

let neg (v : t) : t = match v with
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
  | Symbol s -> invalid_arg "Exception in Val.neg: this operation doesn't apply to Symbol type argument"

let not (v : t) : t = match v with
  | Bool v -> Bool (v = false)
  | _      -> invalid_arg "Exception in Val.not: this operation is only applicable to a boolean type argument"

let plus (v1, v2 : t * t) : t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 +. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 +. v2)
  | (Flt v1, Flt v2) -> Flt (v1 +. v2)
  | (Int v1, Int v2) -> Int (v1 + v2)
  | _                -> invalid_arg "Exception in Val.plus: this operation is only applicable to Float or Int arguments"

let minus (v1, v2 : t * t) : t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 -. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 -. v2)
  | (Flt v1, Flt v2) -> Flt (v1 -. v2)
  | (Int v1, Int v2) -> Int (v1 - v2)
  | _                -> invalid_arg "Exception in Val.minus: this operation is only applicable to Float or Int arguments"

let times (v1, v2 : t * t) : t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 *. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 *. v2)
  | (Flt v1, Flt v2) -> Flt (v1 *. v2)
  | (Int v1, Int v2) -> Int (v1 * v2)
  | _                -> invalid_arg "Exception in Val.times: this operation is only applicable to Float or Int arguments"

let div (v1, v2 : t * t) : t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 /. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 /. v2)
  | (Flt v1, Flt v2) -> Flt (v1 /. v2)
  | (Int v1, Int v2) -> Int (v1 / v2)
  | _                -> invalid_arg "Exception in Val.div: this operation is only applicable to Float or Int arguments"

let equal (v1, v2 : t * t) : t = Bool (v1 = v2)

let gt (v1, v2 : t * t) : t = Bool (v1 > v2)

let lt (v1, v2 : t * t) : t = Bool (v1 < v2)

let egt (v1, v2 : t * t) : t = Bool (v1 >= v2)

let elt (v1, v2 : t * t) : t = Bool (v1 <= v2)

let log_and (v1, v2 : t * t) : t = match v1, v2 with
  | Bool v1, Bool v2 -> Bool (v1 && v2)
  | _                -> invalid_arg "Exception in Val.log_and: this operation is only applicable to Bool arguments"

let log_or (v1, v2 : t * t) : t = match v1, v2 with
  | Bool v1, Bool v2 -> Bool (v1 || v2)
  | _                -> invalid_arg "Exception in Val.log_or: this operation is only applicable to Bool arguments"

let is_true (v : t) : bool = match v with
  | Bool v -> v
  | _      -> invalid_arg "Exception in Val.is_true: argument is not boolean"

let typeof (v : t) : t = match v with
  | Int v  -> Type (Type.IntType)
  | Flt v  -> Type (Type.FltType)
  | Str v  -> Type (Type.StrType)
  | Bool v -> Type (Type.BoolType)
  | Loc v  -> invalid_arg "Exception in Val.typeof: not implemented for Loc type argument"
  | Type v -> invalid_arg "Exception in Val.typeof: not implemented for Type type argument"
  | _      -> invalid_arg "Exception in Val.typeof: invalid argument"

let rec str (v : t) : string = match v with
  | Flt v    -> string_of_float v
  | Int v    -> string_of_int v
  | Bool v   -> string_of_bool v
  | Str v    -> "\"" ^ v ^ "\""
  | Loc v    -> Loc.str v
  | List vs  -> "[" ^ (String.concat ", " (List.map str vs)) ^ "]"
  | Type v   -> Type.str v
  | Void     -> ""
  | Undef    -> "undefined"
  | Null     -> "null"
  | Symbol s -> "'" ^ s

type const = PI

type bopt = Plus
          | Minus
          | Times
          | Div
          | Modulo
          | Equal
          | Gt
          | Lt
          | Egt
          | Elt
          | Log_And
          | Log_Or
          | BitwiseAnd
          | BitwiseOr
          | BitwiseXor
          | ShiftLeft
          | ShiftRight
          | ShiftRightLogical
          | InObj
          | InList
          | Lnth
          | Tnth
          | Ladd
          | Lprepend
          | Lconcat
          | Atan2
          | Max
          | Min
          | Pow

type uopt = Neg
          | Not
          | BitwiseNot
          | Typeof
          | ListLen
          | TupleLen
          | Head
          | Tail
          | First
          | Second
          | IntToFloat
          | IntToString
          | IntOfString
          | FloatOfString
          | FloatToString
          | ObjToList
          | Sconcat
          | ObjFields
          | ToInt
          | ToInt32
          | ToUint32
          | ToUint16
          | Abs
          | Acos
          | Asin
          | Atan
          | Ceil
          | Cos
          | Exp
          | Floor
          | Log_e
          | Log_10
          | Round
          | Random
          | Sin
          | Sqrt
          | Tan


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

let bitwise_not (v : Val.t) : Val.t = match v with
  | Flt f -> Flt (Arith_Utils.int32_bitwise_not f)
  | _     -> invalid_arg "Exception in Oper.bitwise_not: this operation is only applicable to Float arguments"

let plus (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Flt v2) -> Flt (v1 +. v2)
  | (Int v1, Int v2) -> Int (v1 + v2)
  | _                -> invalid_arg "Exception in Oper.plus: this operation is only applicable to Float or Int arguments"

let minus (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Flt v2) -> Flt (v1 -. v2)
  | (Int v1, Int v2) -> Int (v1 - v2)
  | _                -> invalid_arg "Exception in Oper.minus: this operation is only applicable to Float or Int arguments"

let times (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Flt v2) -> Flt (v1 *. v2)
  | (Int v1, Int v2) -> Int (v1 * v2)
  | _                -> invalid_arg "Exception in Oper.times: this operation is only applicable to Float or Int arguments"

let div (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt v1, Flt v2) -> Flt (v1 /. v2)
  | (Int v1, Int v2) -> Int (v1 / v2)
  | _                -> invalid_arg "Exception in Oper.div: this operation is only applicable to Float or Int arguments"

let modulo (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt f1, Flt f2) -> Flt (mod_float f1 f2)
  | _                -> invalid_arg "Exception in Oper.modulo: this operation is only applicable to Float arguments"

let equal (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | (Flt f1, Flt f2) -> Bool (Float.equal f1 f2)
  | _                -> Bool (v1 = v2)

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

let bitwise_and (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Flt f1, Flt f2 -> Flt (Arith_Utils.int32_bitwise_and f1 f2)
  | _              -> invalid_arg "Exception in Oper.bitwise_and: this operation is only applicable to Float arguments"

let bitwise_or (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Flt f1, Flt f2 -> Flt (Arith_Utils.int32_bitwise_or f1 f2)
  | _              -> invalid_arg "Exception in Oper.bitwise_or: this operation is only applicable to Float arguments"

let bitwise_xor (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Flt f1, Flt f2 -> Flt (Arith_Utils.int32_bitwise_xor f1 f2)
  | _              -> invalid_arg "Exception in Oper.bitwise_xor: this operation is only applicable to Float arguments"

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
  | List l -> Val.List (l @ [v2])
  | _      -> invalid_arg "Exception in Oper.list_add: this operation is only applicable to List arguments"

let list_prepend (v1, v2 : Val.t * Val.t) : Val.t =
  Printf.printf "list_prepend: %s" (Val.str v2);
  match v2 with
  | List l -> Val.List (v1::l)
  | _      -> invalid_arg "Exception in Oper.list_prepend: this operation is only applicable to a Value and a List arguments"

let list_concat (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | List l1, List l2 -> Val.List (l1 @ l2)
  | _                -> invalid_arg "Exception in Oper.list_concat: this operation is only applicable to List arguments"

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

let int_to_string (v : Val.t) : Val.t = match v with
  | Int i -> Str (string_of_int i)
  | _     -> invalid_arg "Exception in Oper.int_to_string: this operation is only applicable to Int arguments"

let int_to_float (v : Val.t) : Val.t = match v with
  | Int i -> Flt (float_of_int i)
  | _     -> invalid_arg "Exception in Oper.int_to_float: this operation is only applicable to Int arguments"

let int_of_string (v : Val.t) : Val.t = match v with
  | Str s -> Int (int_of_string s)
  | _     -> invalid_arg "Exception in Oper.int_of_string: this operation is only applicable to Str arguments"

let float_to_string (v : Val.t) : Val.t = match v with
  | Flt i ->
    let s = string_of_float i in
    let len = String.length s - 1 in
    let c = String.get s len in
    let s' = if c = '.' then String.sub s 0 len else s in
    Str s'
  | _     -> invalid_arg ("Exception in Oper.float_to_string: this operation is only applicable to Flt arguments: " ^ (Val.str v))

let float_of_string (v : Val.t) : Val.t = match v with
  | Str s -> (try
                Flt (float_of_string s)
              with _ -> Val.Null)
  | _     -> invalid_arg "Exception in Oper.float_of_string: this operation is only applicable to Str arguments"

let string_concat (v : Val.t) : Val.t = match v with
  | List l -> Str (String.concat "" (String.split_on_char '"' (String.concat "" (List.map Val.str l))))
  | _      -> invalid_arg "Exception in Oper.string_concat: this operation is only applicable to List arguments"

let shift_left (v1, v2: Val.t * Val.t) : Val.t = match v1, v2 with
  | Flt f1, Flt f2 -> Flt (Arith_Utils.int32_left_shift f1 f2)
  | _              -> invalid_arg "Exception in Oper.shift_left: this operation is only applicable to Float arguments"

let shift_right (v1, v2: Val.t * Val.t) : Val.t = match v1, v2 with
  | Flt f1, Flt f2 -> Flt (Arith_Utils.int32_right_shift f1 f2)
  | _              -> invalid_arg "Exception in Oper.shift_right: this operation is only applicable to Float arguments"

let shift_right_logical (v1, v2: Val.t * Val.t) : Val.t = match v1, v2 with
  | Flt f1, Flt f2 -> Flt (Arith_Utils.uint32_right_shift f1 f2)
  | _              -> invalid_arg "Exception in Oper.shift_right_logical: this operation is only applicable to Float arguments"

let to_int (v : Val.t) : Val.t = match v with
  | Flt n -> Flt (Arith_Utils.to_int n)
  | _     -> invalid_arg "Exception in Oper.to_int: this operation is only applicable to Float arguments"

let to_int32 (v : Val.t) : Val.t = match v with
  | Flt n -> Flt (Arith_Utils.to_int32 n)
  | _     -> invalid_arg "Exception in Oper.to_int32: this operation is only applicable to Float arguments"

let to_uint32 (v : Val.t) : Val.t = match v with
  | Flt n -> Flt (Arith_Utils.to_uint32 n)
  | _     -> invalid_arg "Exception in Oper.to_uint32: this operation is only applicable to Float arguments"

let to_uint16 (v : Val.t) : Val.t = match v with
  | Flt n -> Flt (Arith_Utils.to_uint16 n)
  | _     -> invalid_arg "Exception in Oper.to_uint16: this operation is only applicable to Float arguments"


let str_of_const (c : const) : string = match c with
  | PI -> "PI"

let str_of_unopt (op : uopt) : string = match op with
  | Neg           -> "-"
  | Not           -> "!"
  | BitwiseNot    -> "~"
  | Typeof        -> "typeof"
  | ListLen       -> "l_len"
  | TupleLen      -> "t_len"
  | Head          -> "hd"
  | Tail          -> "tl"
  | First         -> "fst"
  | Second        -> "snd"
  | IntToFloat    -> "int_to_float"
  | IntToString   -> "int_to_string"
  | IntOfString   -> "int_of_string"
  | FloatOfString -> "float_of_string"
  | FloatToString -> "float_to_string"
  | ObjToList     -> "obj_to_list"
  | Sconcat       -> "s_concat"
  | ObjFields     -> "obj_fields"
  | ToInt         -> "to_int"
  | ToInt32       -> "to_int32"
  | ToUint32      -> "to_uint32"
  | ToUint16      -> "to_uint16"
  | Abs           -> "abs"
  | Acos          -> "acos"
  | Asin          -> "asin"
  | Atan          -> "atan"
  | Ceil          -> "ceil"
  | Cos           -> "cos"
  | Exp           -> "exp"
  | Floor         -> "floor"
  | Log_e         -> "log_e"
  | Log_10        -> "log_10"
  | Round         -> "round"
  | Random        -> "random"
  | Sin           -> "sin"
  | Sqrt          -> "sqrt"
  | Tan           -> "tan"

let str_of_binopt (op : bopt) (e1 : string) (e2 : string) : string = match op with
  | Plus     -> e1 ^ " + " ^ e2
  | Minus    -> e1 ^ " - " ^ e2
  | Times    -> e1 ^ " * " ^ e2
  | Div      -> e1 ^ " / " ^ e2
  | Modulo   -> e1 ^ " % " ^ e2
  | Equal    -> e1 ^ " = " ^ e2
  | Gt       -> e1 ^ " > " ^ e2
  | Lt       -> e1 ^ " < " ^ e2
  | Egt      -> e1 ^ " >= " ^ e2
  | Elt      -> e1 ^ " <= " ^ e2
  | Log_And  -> e1 ^ " && " ^ e2
  | Log_Or   -> e1 ^ " || " ^ e2
  | BitwiseAnd  -> e1 ^ " & " ^ e2
  | BitwiseOr  -> e1 ^ " | " ^ e2
  | BitwiseXor  -> e1 ^ " ^ " ^ e2
  | ShiftLeft -> e1 ^ " << " ^ e2
  | ShiftRight -> e1 ^ " >> " ^ e2
  | ShiftRightLogical -> e1 ^ " >>> " ^ e2
  | InObj    -> e1 ^ " in_obj " ^ e2
  | InList   -> e1 ^ " in_list " ^ e2
  | Lnth     -> "l_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Tnth     -> "t_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Ladd     -> "l_add(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Lprepend -> "l_prepend(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Lconcat  -> "l_concat(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Atan2    -> "atan2(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Max      -> "max(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Min      -> "min(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Pow      -> e1 ^ " ** " ^ e2

let str_of_nopt (op : nopt) (es : string list) : string = match op with
  | ListExpr  -> "[ " ^ (String.concat ", " es) ^ " ]"
  | TupleExpr -> "( " ^ (String.concat ", " es) ^ " )"
  | NAry_And  -> String.concat " && " es
  | NAry_Or   -> String.concat " || " es


let unary_float_call (func : float -> float) (v : Val.t) (failure_msg : string) : Val.t = match v with
  | Flt f -> Flt (func f)
  | _     -> invalid_arg (Printf.sprintf "Exception in %s: expected float, got %s" failure_msg (Val.str v))

let binary_float_call (func : float -> float -> float) (v1 : Val.t) (v2 : Val.t) (failure_msg : string) : Val.t = match v1, v2 with
  | Flt f1, Flt f2 -> Flt (func f1 f2)
  | _              -> invalid_arg (Printf.sprintf "Exception in %s: expected floats, got %s and %s" failure_msg (Val.str v1) (Val.str v2))

let apply_uopt_oper (oper : uopt) (v : Val.t) : Val.t = match oper with
  | Abs    -> unary_float_call Float.abs v    "Absolute value"
  | Acos   -> unary_float_call Float.acos v   "Arc cosine"
  | Asin   -> unary_float_call Float.asin v   "Arc sine"
  | Atan   -> unary_float_call Float.atan v   "Arc tangent"
  | Ceil   -> unary_float_call Float.ceil v   "Ceil"
  | Cos    -> unary_float_call Float.cos v    "Cosine"
  | Exp    -> unary_float_call Float.exp v    "Exponential"
  | Floor  -> unary_float_call Float.floor v  "Floor"
  | Log_e  -> unary_float_call Float.log v    "Natural logarithm"
  | Log_10 -> unary_float_call Float.log10 v  "Base-10 logarithm"
  | Round  -> unary_float_call Float.round v  "Round"
  | Random -> unary_float_call Random.float v "Random"
  | Sin    -> unary_float_call Float.sin v    "Sine"
  | Sqrt   -> unary_float_call Float.sqrt v   "Square root"
  | Tan    -> unary_float_call Float.tan v    "Tangent"
  | _      -> invalid_arg ("Exception in Oper.apply_uopt_oper: unexpected unary operator: " ^ (str_of_unopt oper))

let apply_bopt_oper (oper : bopt) (v1 : Val.t) (v2 : Val.t) : Val.t = match oper with
  | Atan2 -> binary_float_call Float.atan2 v1 v2 "Arc tangent of quotient y/x"
  | Max   -> binary_float_call Float.max v1 v2   "Max"
  | Min   -> binary_float_call Float.min v1 v2   "Min"
  | Pow   -> binary_float_call Float.pow v1 v2   "Power"
  | _     -> invalid_arg ("Exception in Oper.apply_bopt_oper: unexpected binary operator: " ^ (str_of_binopt oper (Val.str v1) (Val.str v2)))


let bopt_to_json (op : bopt) : string =
  Printf.sprintf "{ \"type\" : \"binopt\", \"value\" : \"%s"
    (match op with
     | Plus    -> Printf.sprintf "Plus\" }"
     | Minus   -> Printf.sprintf "Minus\" }"
     | Times   -> Printf.sprintf "Times\" }"
     | Div     -> Printf.sprintf "Div\" }"
     | Modulo  -> Printf.sprintf "Modulo\" }"
     | Equal   -> Printf.sprintf "Equal\" }"
     | Gt      -> Printf.sprintf "Gt\" }"
     | Lt      -> Printf.sprintf "Lt\" }"
     | Egt     -> Printf.sprintf "Egt\" }"
     | Elt     -> Printf.sprintf "Elt\" }"
     | Log_And -> Printf.sprintf "Log_And\" }"
     | Log_Or  -> Printf.sprintf "Log_Or\" }"
     | BitwiseAnd -> Printf.sprintf "BitwiseAnd\" }"
     | BitwiseOr -> Printf.sprintf "BitwiseOr\" }"
     | BitwiseXor -> Printf.sprintf "BitwiseXor\" }"
     | ShiftLeft -> Printf.sprintf "ShiftLeft\" }"
     | ShiftRight -> Printf.sprintf "ShiftRight\" }"
     | ShiftRightLogical -> Printf.sprintf "ShiftRightLogical\" }"
     | InObj   -> Printf.sprintf "InObj\" }"
     | InList  -> Printf.sprintf "InList\" }"
     | Lnth    -> Printf.sprintf "Lnth\" }"
     | Tnth    -> Printf.sprintf "Tnth\" }"
     | Ladd    -> Printf.sprintf "Ladd\" }"
     | Lprepend -> Printf.sprintf "Lprepend\" }"
     | Lconcat  -> Printf.sprintf "Lconcat\" }"
     | Atan2    -> Printf.sprintf "Atan2\" }"
     | Max      -> Printf.sprintf "Max\" }"
     | Min      -> Printf.sprintf "Min\" }"
     | Pow      -> Printf.sprintf "Pow\" }")

let nopt_to_json (op : nopt) : string =
  Printf.sprintf "{ \"type\" : \"nopt\", \"value\" : \"%s"
    (match op with
     | ListExpr -> Printf.sprintf "ListExpr\" }"
     | TupleExpr -> Printf.sprintf "TupleExpr\" }"
     | NAry_And -> Printf.sprintf "NAry_And\" }"
     | NAry_Or -> Printf.sprintf "NAry_Or\" }")


let uopt_to_json (op : uopt) : string =
  Printf.sprintf "{ \"type\" : \"unopt\", \"value\" : \"%s"
    (match op with
     | Neg           -> Printf.sprintf "Neg\" }"
     | Not           -> Printf.sprintf "Not\" }"
     | BitwiseNot    -> Printf.sprintf "BitwiseNot\" }"
     | Typeof        -> Printf.sprintf "Typeof\" }"
     | ListLen       -> Printf.sprintf "ListLen\" }"
     | TupleLen      -> Printf.sprintf "TypleLen\" }"
     | Head          -> Printf.sprintf "Head\" }"
     | Tail          -> Printf.sprintf "Tail\" }"
     | First         -> Printf.sprintf "First\" }"
     | Second        -> Printf.sprintf "Second\" }"
     | IntToFloat    -> Printf.sprintf "IntToFloat\" }"
     | IntToString   -> Printf.sprintf "IntToString\" }"
     | IntOfString   -> Printf.sprintf "IntOfString\" }"
     | FloatOfString -> Printf.sprintf "FloatOfString\" }"
     | FloatToString -> Printf.sprintf "FloatToString\" }"
     | ObjToList     -> Printf.sprintf "ObjToList\" }"
     | Sconcat       -> Printf.sprintf "Sconcat\" }"
     | ObjFields     -> Printf.sprintf "ObjFields\" }"
     | ToInt         -> Printf.sprintf "ToInt\" }"
     | ToInt32       -> Printf.sprintf "ToInt32\" }"
     | ToUint32      -> Printf.sprintf "ToUint32\" }"
     | ToUint16      -> Printf.sprintf "ToUint16\" }"
     | Abs           -> Printf.sprintf "Abs\" }"
     | Acos          -> Printf.sprintf "Acos\" }"
     | Asin          -> Printf.sprintf "Asin\" }"
     | Atan          -> Printf.sprintf "Atan\" }"
     | Ceil          -> Printf.sprintf "Ceil\" }"
     | Cos           -> Printf.sprintf "Cos\" }"
     | Exp           -> Printf.sprintf "Exp\" }"
     | Floor         -> Printf.sprintf "Floor\" }"
     | Log_e         -> Printf.sprintf "Log_e\" }"
     | Log_10        -> Printf.sprintf "Log_10\" }"
     | Round         -> Printf.sprintf "Round\" }"
     | Random        -> Printf.sprintf "Random\" }"
     | Sin           -> Printf.sprintf "Sin\" }"
     | Sqrt          -> Printf.sprintf "Sqrt\" }"
     | Tan           -> Printf.sprintf "Tan\" }")


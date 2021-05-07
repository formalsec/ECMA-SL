type const = MAX_VALUE
           | MIN_VALUE
           | PI

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
          | Snth
          | Snth_u
          | Ssplit
          | Ladd
          | Lprepend
          | Lconcat
          | Atan2
          | Max
          | Min
          | Pow

type topt = Ssubstr
          | SsubstrU

type uopt = Neg
          | Not
          | BitwiseNot
          | Typeof
          | ListLen
          | TupleLen
          | StringLen
          | StringLenU
          | Head
          | Tail
          | First
          | Second
          | LRemoveLast
          | LSort
          | IntToFloat
          | IntToString
          | IntToFourHex
          | IntOfString
          | IntOfFloat
          | FloatOfString
          | FloatToString
          | HexDecode
          | Utf8Decode
          | OctalToDecimal
          | ObjToList
          | Sconcat
          | ObjFields
          | ToInt
          | ToInt32
          | ToUint32
          | ToUint16
          | FromCharCode
          | FromCharCodeU
          | ToCharCode
          | ToCharCodeU
          | ToLowerCase
          | ToUpperCase
          | Trim
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
  | Curry _  -> Type (Type.CurryType)
  | Void     -> invalid_arg ("Exception in Oper.typeof: unexpected void value")

let l_len (v : Val.t) : Val.t = match v with
  | List l -> Val.Int (List.length l)
  | _      -> invalid_arg "Exception in Oper.l_len: this operation is only applicable to List arguments"

let t_len (v : Val.t) : Val.t = match v with
  | Tuple t -> Val.Int (List.length t)
  | _       -> invalid_arg "Exception in Oper.t_len: this operation is only applicable to Tuple arguments"

let s_len (v : Val.t) : Val.t = match v with
  | Str s -> Int (String.length s)
  | _     -> invalid_arg "Exception in Oper.s_len: this operation is only applicable to String arguments"

let s_len_u (v : Val.t) : Val.t = match v with
  | Str s -> Int (String_Utils.s_len_u s)
  | _     -> invalid_arg "Exception in Oper.s_len_u: this operation is only applicable to String arguments"

let list_nth (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | List l, Int i -> List.nth l i
  | _             -> invalid_arg "Exception in Oper.list_nth: this operation is only applicable to List and Int arguments"

let tuple_nth (v1, v2 : Val.t * Val.t) : Val.t = match v1, v2 with
  | Tuple l, Int i -> List.nth l i
  | _              -> invalid_arg "Exception in Oper.tuple_nth: this operation is only applicable to Tuple and Int arguments"

let s_nth (v1, v2: Val.t * Val.t) : Val.t = match v1, v2 with
  | Str s, Int i -> Str (String.sub s i 1)
  | _            -> invalid_arg "Exception in Oper.s_nth: this operation is only applicable to String and Integer arguments"

let s_nth_u (v1, v2: Val.t * Val.t) : Val.t = match v1, v2 with
  | Str s, Int i -> Str (String_Utils.s_nth_u s i)
  | _            -> invalid_arg "Exception in Oper.s_nth_u: this operation is only applicable to String and Integer arguments"

let s_substr (v1, v2, v3: Val.t * Val.t * Val.t) : Val.t = match v1, v2, v3 with
  | Str s, Int i, Int j -> Str (String.sub s i j)
  | _                   -> invalid_arg "Exception in Oper.s_substr: this operation is only applicable to String and two Integer arguments"

let s_substr_u (v1, v2, v3: Val.t * Val.t * Val.t) : Val.t = match v1, v2, v3 with
  | Str s, Int i, Int j -> Str (String_Utils.s_substr_u s i j)
  | _                   -> invalid_arg "Exception in Oper.s_substr_u: this operation is only applicable to String and two Integer arguments"

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

let list_remove_last (v : Val.t) : Val.t = match v with
| List l  ->
  let l' = List.rev l in
  (match l' with
    | _ :: l'' -> List (List.rev l'')
    | _ -> List [])
| _       -> invalid_arg "Exception in Oper.list_remove_last: this operation is only applicable to List arguments"

let list_sort (v : Val.t) : Val.t = match v with
  | List l ->
    let strs =
      List.fold_left
        (fun acc v ->
           match acc, v with
           | Some strs, Val.Str s -> Some (strs @ [s])
           | _                    -> None)
        (Some [])
        l in
    (match strs with
     | None      -> invalid_arg "Exception in Oper.list_sort: this operation is only applicable to List of string arguments"
     | Some strs -> List (List.map (fun s -> Val.Str s) (List.fast_sort (String.compare) strs)))
  | _      -> invalid_arg "Exception in Oper.list_sort: this operation is only applicable to List arguments"

let first (v : Val.t) : Val.t = match v with
  | Tuple t -> List.hd t
  | _       -> invalid_arg "Exception in Oper.first: this operation is only applicable to Tuple arguments"

let second (v : Val.t) : Val.t = match v with
  | Tuple t -> List.nth t 1
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

let int_of_float (v : Val.t) : Val.t = match v with
  | Flt f -> Int (int_of_float f)
  | _     -> invalid_arg "Exception in Oper.int_of_float: this operation is only applicable to Flt arguments."

let float_to_string (v : Val.t) : Val.t = match v with
  | Flt i -> Str (Arith_Utils.float_to_string_inner i)
  | _     -> invalid_arg ("Exception in Oper.float_to_string: this operation is only applicable to Flt arguments: " ^ (Val.str v))

let float_of_string (v : Val.t) : Val.t = match v with
  | Str s -> (if String.length (String.trim s) == 0
              then Flt 0.
              else (try Flt (float_of_string s) with _ -> Flt nan))
  | _     -> invalid_arg "Exception in Oper.float_of_string: this operation is only applicable to Str arguments"

let string_concat (v : Val.t) : Val.t = match v with
  | List l ->
    let strs =
      List.fold_left
        (fun acc v ->
           match acc, v with
           | Some strs, Val.Str s -> Some (strs @ [s])
           | _                    -> None)
        (Some [])
        l in
    (match strs with
     | None      -> invalid_arg "Exception.Oper.string_concat: this operation is only applicable to List of string arguments"
     | Some strs -> Str (String.concat "" strs))
  | _      -> invalid_arg "Exception in Oper.string_concat: this operation is only applicable to List arguments"

let string_split (v, c : Val.t * Val.t) : Val.t = match v, c with
  | _, Str ""        -> invalid_arg "Exception in Oper.string_split: separator cannot be the empty string"
  | Str str, Str sep ->
    let c = String.get sep 0 in
    Val.List (List.map (fun str -> Val.Str str) (String.split_on_char c str))
  | _                -> invalid_arg "Exception in Oper.string_split: this operation is only applicable to String arguments"

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

let from_char_code (v : Val.t) : Val.t = match v with
  | Int n -> Str (String_Utils.from_char_code n)
  | _     -> invalid_arg "Exception in Oper.from_char_code: this operation is only applicable to Int arguments"

let from_char_code_u (v : Val.t) : Val.t = match v with
  | Int n -> Str (String_Utils.from_char_code_u n)
  | _     -> invalid_arg "Exception in Oper.from_char_code_u: this operation is only applicable to Int arguments"

let to_char_code (v : Val.t) : Val.t = match v with
  | Str s -> Int (String_Utils.to_char_code s)
  | _     -> invalid_arg "Exception in Oper.to_char_code: this operation is only applicable to Str arguments"

let to_char_code_u (v : Val.t) : Val.t = match v with
  | Str s -> Int (String_Utils.to_char_code_u s)
  | _     -> invalid_arg "Exception in Oper.to_char_code_u: this operation is only applicable to Str arguments"

let int_to_four_hex (v : Val.t) : Val.t = match v with
| Int i -> Str (Printf.sprintf "%04x" i)
| _     -> invalid_arg "Exception in Oper.int_to_four_hex: this operation is only applicable to Int arguments"

let utf8_decode (v : Val.t) : Val.t = match v with
  | Str s -> Str(String_Utils.utf8decode s)
  | _     -> invalid_arg "Exception in Oper.utf8_decode: this operation is only applicable to Str arguments"

let hex_decode (v : Val.t) : Val.t = match v with
  | Str s -> Str (String_Utils.hexdecode s)
  | _     -> invalid_arg "Exception in Oper.hex_decode: this operation is only applicable to Str arguments"

let octal_to_decimal (v : Val.t) : Val.t = match v with
  | Int o ->
    let rec loop dec_value base temp =
      if temp = 0 then dec_value
      else let dec_value = dec_value + ((temp mod 10) * base) in
        loop dec_value (base * 8) (temp / 10) in
    Int(loop 0 1 o)
  | _     -> invalid_arg "Exception in Oper.octal_to_decimal: this operation is only applicable to Int arguments"

let to_lower_case (v : Val.t) : Val.t = match v with
  | Str s -> Str (String_Utils.to_lower_case s)
  | _     -> invalid_arg "Exception in Oper.to_lower_case: this operation is only applicable to Str arguments"

let to_upper_case (v : Val.t) : Val.t = match v with
  | Str s -> Str (String_Utils.to_upper_case s)
  | _     -> invalid_arg "Exception in Oper.to_upper_case: this operation is only applicable to Str arguments"

let trim (v : Val.t) : Val.t = match v with
  | Str s -> Str (String_Utils.trim s)
  | _     -> invalid_arg "Exception in Oper.trim: this operation is only applicable to Str arguments"


let str_of_const (c : const) : string = match c with
  | MAX_VALUE -> "MAX_VALUE"
  | MIN_VALUE -> "MIN_VALUE"
  | PI        -> "PI"

let str_of_unopt (op : uopt) : string = match op with
  | Neg           -> "-"
  | Not           -> "!"
  | BitwiseNot    -> "~"
  | Typeof        -> "typeof"
  | ListLen       -> "l_len"
  | TupleLen      -> "t_len"
  | StringLen     -> "s_len"
  | StringLenU    -> "s_len_u"
  | Head          -> "hd"
  | Tail          -> "tl"
  | First         -> "fst"
  | Second        -> "snd"
  | LRemoveLast   -> "l_remove_last"
  | LSort         -> "l_sort"
  | IntToFloat    -> "int_to_float"
  | IntToString   -> "int_to_string"
  | IntToFourHex  -> "int_to_four_hex"
  | IntOfString   -> "int_of_string"
  | IntOfFloat    -> "int_of_float"
  | FloatOfString -> "float_of_string"
  | FloatToString -> "float_to_string"
  | HexDecode     -> "hex_decode"
  | Utf8Decode    -> "utf8_decode"
  | OctalToDecimal-> "octal_to_decimal"
  | ObjToList     -> "obj_to_list"
  | Sconcat       -> "s_concat"
  | ObjFields     -> "obj_fields"
  | ToInt         -> "to_int"
  | ToInt32       -> "to_int32"
  | ToUint32      -> "to_uint32"
  | ToUint16      -> "to_uint16"
  | FromCharCode  -> "from_char_code"
  | FromCharCodeU -> "from_char_code_u"
  | ToCharCode    -> "to_char_code"
  | ToCharCodeU   -> "to_char_code_u"
  | ToLowerCase   -> "to_lower_case"
  | ToUpperCase   -> "to_upper_case"
  | Trim          -> "trim"
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
  | Random        -> "random"
  | Sin           -> "sin"
  | Sqrt          -> "sqrt"
  | Tan           -> "tan"


let str_of_binopt_single (op : bopt) : string = match op with
  | Plus     -> "+"
  | Minus    -> "-"
  | Times    -> "*"
  | Div      -> "/"
  | Modulo   -> "%"
  | Equal    -> "="
  | Gt       -> ">"
  | Lt       -> "<"
  | Egt      -> ">="
  | Elt      -> "<="
  | Log_And  -> "&&"
  | Log_Or   -> "||"
  | BitwiseAnd  -> "&"
  | BitwiseOr  -> "|"
  | BitwiseXor  -> "^"
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | ShiftRightLogical -> ">>>"
  | InObj    -> "in_obj"
  | InList   -> "in_list"
  | Lnth     -> "l_nth"
  | Tnth     -> "t_nth"
  | Snth     -> "s_nth"
  | Snth_u   -> "s_nth_u"
  | Ssplit   -> "s_split"
  | Ladd     -> "l_add"
  | Lprepend -> "l_prepend"
  | Lconcat  -> "l_concat"
  | Atan2    -> "atan2"
  | Max      -> "max"
  | Min      -> "min"
  | Pow      -> "**"

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
  | Snth     -> "s_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Snth_u   -> "s_nth_u(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Ssplit   -> Printf.sprintf "s_split(%s, %s)" e1 e2
  | Ladd     -> "l_add(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Lprepend -> "l_prepend(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Lconcat  -> "l_concat(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Atan2    -> "atan2(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Max      -> "max(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Min      -> "min(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Pow      -> e1 ^ " ** " ^ e2

let str_of_triopt (op : topt) (e1 : string) (e2 : string) (e3 : string) : string = match op with
  | Ssubstr  -> "s_substr(" ^ e1 ^ ", " ^ e2 ^ ", " ^ e3 ^ ")"
  | SsubstrU  -> "s_substr_u(" ^ e1 ^ ", " ^ e2 ^ ", " ^ e3 ^ ")"

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
     | Snth    -> Printf.sprintf "Snth\" }"
     | Snth_u  -> Printf.sprintf "Snth_u\" }"
     | Ssplit  -> Printf.sprintf "Ssplit\" }"
     | Ladd    -> Printf.sprintf "Ladd\" }"
     | Lprepend -> Printf.sprintf "Lprepend\" }"
     | Lconcat  -> Printf.sprintf "Lconcat\" }"
     | Atan2    -> Printf.sprintf "Atan2\" }"
     | Max      -> Printf.sprintf "Max\" }"
     | Min      -> Printf.sprintf "Min\" }"
     | Pow      -> Printf.sprintf "Pow\" }")

let topt_to_json (op : topt) : string =
  Printf.sprintf "{ \"type\" : \"triopt\", \"value\" : \"%s"
    (match op with
      | Ssubstr -> Printf.sprintf "Ssubstr\" }"
      | SsubstrU -> Printf.sprintf "SsubstrU\" }")

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
     | StringLen     -> Printf.sprintf "StringLen\" }"
     | StringLenU    -> Printf.sprintf "StringLenU\" }"
     | Head          -> Printf.sprintf "Head\" }"
     | Tail          -> Printf.sprintf "Tail\" }"
     | First         -> Printf.sprintf "First\" }"
     | Second        -> Printf.sprintf "Second\" }"
     | LRemoveLast   -> Printf.sprintf "LRemoveLast\" }"
     | LSort         -> Printf.sprintf "LSort\" }"
     | IntToFloat    -> Printf.sprintf "IntToFloat\" }"
     | IntToString   -> Printf.sprintf "IntToString\" }"
     | IntToFourHex  -> Printf.sprintf "IntToFourHex\" }"
     | IntOfString   -> Printf.sprintf "IntOfString\" }"
     | IntOfFloat    -> Printf.sprintf "IntOfFloat\" }"
     | FloatOfString -> Printf.sprintf "FloatOfString\" }"
     | FloatToString -> Printf.sprintf "FloatToString\" }"
     | HexDecode     -> Printf.sprintf "HexDecode\" }"
     | Utf8Decode    -> Printf.sprintf "Utf8Decode\" }"
     | OctalToDecimal-> Printf.sprintf "OctalToDecimal\" }"
     | ObjToList     -> Printf.sprintf "ObjToList\" }"
     | Sconcat       -> Printf.sprintf "Sconcat\" }"
     | ObjFields     -> Printf.sprintf "ObjFields\" }"
     | ToInt         -> Printf.sprintf "ToInt\" }"
     | ToInt32       -> Printf.sprintf "ToInt32\" }"
     | ToUint32      -> Printf.sprintf "ToUint32\" }"
     | ToUint16      -> Printf.sprintf "ToUint16\" }"
     | FromCharCode  -> Printf.sprintf "FromCharCode\" }"
     | FromCharCodeU -> Printf.sprintf "FromCharCodeU\" }"
     | ToCharCode    -> Printf.sprintf "ToCharCode\" }"
     | ToCharCodeU   -> Printf.sprintf "ToCharCodeU\" }"
     | ToLowerCase   -> Printf.sprintf "ToLowerCase\" }"
     | ToUpperCase   -> Printf.sprintf "ToUpperCase\" }"
     | Trim          -> Printf.sprintf "Trim\" }"
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
     | Random        -> Printf.sprintf "Random\" }"
     | Sin           -> Printf.sprintf "Sin\" }"
     | Sqrt          -> Printf.sprintf "Sqrt\" }"
     | Tan           -> Printf.sprintf "Tan\" }")


open Val

type const = MAX_VALUE | MIN_VALUE | PI

type bopt =
  | Plus
  | Minus
  | Times
  | Div
  | Modulo
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
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
  | LRem
  | LRemNth
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
  | ToPrecision
  | ToExponential
  | ToFixed
  | ArrayMake
  | Anth
  | IntToBEBytes
  | IntFromBytes
  | UintFromBytes

type topt = Ssubstr | SsubstrU | Aset | Lset | ITE

type uopt =
  | Neg
  | Not
  | IsNaN
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
  | LReverse
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
  | ParseNumber
  | ParseString
  | ParseDate
  | Cosh
  | Log_2
  | Sinh
  | Tanh
  | Float64ToLEBytes
  | Float64ToBEBytes
  | Float32ToLEBytes
  | Float32ToBEBytes
  | Float64FromLEBytes
  | Float64FromBEBytes
  | Float32FromLEBytes
  | Float32FromBEBytes
  | BytesToString
  | FloatToByte
  | ArrayLen
  | ListToArray

type nopt = ListExpr | TupleExpr | NAry_And | NAry_Or | ArrExpr

let neg (v : Val.t) : Val.t =
  match v with
  | Flt v -> Flt (-.v)
  | Int v -> Int (-v)
  | _ ->
      invalid_arg
        "Exception in Oper.neg: this operation is only applicable to Float or \
         Int arguments"

let not (v : Val.t) : Val.t =
  match v with
  | Bool v -> Bool (not v)
  | _ ->
      invalid_arg
        "Exception in Oper.not: this operation is only applicable to a boolean \
         type argument"

let is_NaN (v : Val.t) : Val.t =
  match v with Flt v -> Bool (Float.is_nan v) | _ -> Bool false

let bitwise_not (v : Val.t) : Val.t =
  match v with
  | Flt f -> Flt (Arith_utils.int32_bitwise_not f)
  | _ ->
      invalid_arg
        "Exception in Oper.bitwise_not: this operation is only applicable to \
         Float arguments"

let plus ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt v1, Flt v2 -> Flt (v1 +. v2)
  | Int v1, Int v2 -> Int (v1 + v2)
  | _ ->
      invalid_arg
        "Exception in Oper.plus: this operation is only applicable to Float or \
         Int arguments"

let minus ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt v1, Flt v2 -> Flt (v1 -. v2)
  | Int v1, Int v2 -> Int (v1 - v2)
  | _ ->
      invalid_arg
        "Exception in Oper.minus: this operation is only applicable to Float \
         or Int arguments"

let times ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt v1, Flt v2 -> Flt (v1 *. v2)
  | Int v1, Int v2 -> Int (v1 * v2)
  | _ ->
      invalid_arg
        "Exception in Oper.times: this operation is only applicable to Float \
         or Int arguments"

let div ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt v1, Flt v2 -> Flt (v1 /. v2)
  | Int v1, Int v2 -> Int (v1 / v2)
  | _ ->
      invalid_arg
        "Exception in Oper.div: this operation is only applicable to Float or \
         Int arguments"

let modulo ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Flt (mod_float f1 f2)
  | _ ->
      invalid_arg
        "Exception in Oper.modulo: this operation is only applicable to Float \
         arguments"

let equal ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Bool (Float.equal f1 f2)
  | Arr a1, Arr a2 -> Bool (a1 == a2)
  | _ -> Bool (v1 = v2)

let gt ((v1, v2) : Val.t * Val.t) : Val.t = Bool (v1 > v2)
let lt ((v1, v2) : Val.t * Val.t) : Val.t = Bool (v1 < v2)
let egt ((v1, v2) : Val.t * Val.t) : Val.t = Bool (v1 >= v2)
let elt ((v1, v2) : Val.t * Val.t) : Val.t = Bool (v1 <= v2)

let log_and ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Bool v1, Bool v2 -> Bool (v1 && v2)
  | _ ->
      invalid_arg
        "Exception in Oper.log_and: this operation is only applicable to Bool \
         arguments"

let log_or ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Bool v1, Bool v2 -> Bool (v1 || v2)
  | _ ->
      invalid_arg
        "Exception in Oper.log_or: this operation is only applicable to Bool \
         arguments"

let bitwise_and ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Flt (Arith_utils.int32_bitwise_and f1 f2)
  | _ ->
      invalid_arg
        "Exception in Oper.bitwise_and: this operation is only applicable to \
         Float arguments"

let bitwise_or ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Flt (Arith_utils.int32_bitwise_or f1 f2)
  | _ ->
      invalid_arg
        "Exception in Oper.bitwise_or: this operation is only applicable to \
         Float arguments"

let bitwise_xor ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Flt (Arith_utils.int32_bitwise_xor f1 f2)
  | _ ->
      invalid_arg
        "Exception in Oper.bitwise_xor: this operation is only applicable to \
         Float arguments"

let is_true (v : Val.t) : bool =
  match v with
  | Bool v -> v
  | _ -> invalid_arg "Exception in Oper.is_true: argument is not boolean"

let to_precision ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt x, Int y ->
      let z = Float.to_int (Float.log10 x) + 1 in
      if y < z then
        let exp = Float.log10 x in
        if exp >= 0. then
          let num =
            Float.round
              (x /. (10. ** Float.trunc exp) *. (10. ** Float.of_int (y - 1)))
            /. (10. ** Float.of_int (y - 1))
          in
          if Float.is_integer num && y = 1 then
            Str
              (string_of_int (Float.to_int num)
              ^ "e+"
              ^ Int.to_string (Float.to_int exp))
          else
            Str (string_of_float num ^ "e+" ^ Int.to_string (Float.to_int exp))
        else
          let num =
            Float.round
              (x /. (10. ** Float.floor exp) *. (10. ** Float.of_int (y - 1)))
            /. (10. ** Float.of_int (y - 1))
          in
          if Float.is_integer num && y = 1 then
            Str
              (string_of_int (Float.to_int num)
              ^ "e"
              ^ Int.to_string (Float.to_int (Float.floor exp)))
          else
            Str
              (string_of_float num ^ "e"
              ^ Int.to_string (Float.to_int (Float.floor exp)))
      else
        let res =
          Float.round (x *. (10. ** float_of_int (y - 1)))
          /. (10. ** float_of_int (y - 1))
        in
        Str (Float.to_string res)
  | _ ->
      invalid_arg
        "Exception in Oper.to_precision: this operation is only applicable to \
         Float and Int arguments"

let to_exponential ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt x, Int y ->
      let exp = Float.log10 x in
      (* Printf.printf "list_prepend: %s" (Val.str v2); *)
      Printf.printf "to_exponential: %s\n" (Float.to_string exp);
      Printf.printf "to_exponential: %s\n"
        (Float.to_string (10. ** Float.trunc exp));
      Printf.printf "to_exponential: %s\n"
        (Float.to_string (x /. (10. ** Float.trunc exp)));
      Printf.printf "to_exponential: %s\n"
        (Float.to_string (10. ** Float.of_int y));
      Printf.printf "to_exponential: %s\n"
        (Float.to_string ((10. ** Float.of_int y) /. (10. ** Float.of_int y)));
      Printf.printf "to_exponential: %s\n"
        (Float.to_string
           (x
           /. (10. ** Float.trunc exp)
           *. (10. ** Float.of_int y)
           /. (10. ** Float.of_int y)));
      if exp >= 0. then
        let num =
          Float.round (x /. (10. ** Float.trunc exp) *. (10. ** Float.of_int y))
          /. (10. ** Float.of_int y)
        in
        if Float.is_integer num then
          Str
            (string_of_int (Float.to_int num)
            ^ "e+"
            ^ Int.to_string (Float.to_int exp))
        else Str (string_of_float num ^ "e+" ^ Int.to_string (Float.to_int exp))
      else
        let num =
          Float.round (x /. (10. ** Float.floor exp) *. (10. ** Float.of_int y))
          /. (10. ** Float.of_int y)
        in
        if Float.is_integer num then
          Str
            (string_of_int (Float.to_int num)
            ^ "e"
            ^ Int.to_string (Float.to_int (Float.floor exp)))
        else
          Str
            (string_of_float num ^ "e"
            ^ Int.to_string (Float.to_int (Float.floor exp)))
  | _ ->
      invalid_arg
        "Exception in Oper.to_exponential: this operation is only applicable \
         to Float and Int arguments"

let to_fixed ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt x, Int y ->
      (* let res = Float.round(x*.(10.**(Float.of_int(y))))/.(10.**(Float.of_int(y))) in *)
      (* let digits = Arith_utils.count_digits res in *)
      (* let missing_zeros = y - digits in *)
      (* Str (Float.to_string res) *)
      Str (Printf.sprintf "%0.*f" y x)
  | _ ->
      invalid_arg
        "Exception in Oper.to_fixed: this operation is only applicable to \
         Float and Int arguments"

let typeof (v : Val.t) : Val.t =
  match v with
  | Int _ -> Type Type.IntType
  | Flt _ -> Type Type.FltType
  | Bool _ -> Type Type.BoolType
  | Str _ -> Type Type.StrType
  | Loc _ -> Type Type.LocType
  | List _ -> Type Type.ListType
  | Arr _ -> Type Type.ArrayType
  | Type _ -> Type Type.TypeType
  | Tuple _ -> Type Type.TupleType
  | Null -> Type Type.NullType
  | Symbol _ -> Type Type.SymbolType
  | Curry _ -> Type Type.CurryType
  | Void -> invalid_arg "Exception in Oper.typeof: unexpected void value"
  | Byte _ -> invalid_arg "Type of Byte not implemented yet"
(*  | Byte32 _   -> invalid_arg ("Type of Byte32 not implemented yet") *)

let l_len (v : Val.t) : Val.t =
  match v with
  | List l -> Val.Int (List.length l)
  | _ ->
      invalid_arg
        "Exception in Oper.l_len: this operation is only applicable to List \
         arguments"

let a_len (v : Val.t) : Val.t =
  match v with
  | Arr l -> Val.Int (Array.length l)
  | _ ->
      invalid_arg
        "Exception in Oper.a_len: this operation is only applicable to Array \
         arguments"

let t_len (v : Val.t) : Val.t =
  match v with
  | Tuple t -> Val.Int (List.length t)
  | _ ->
      invalid_arg
        "Exception in Oper.t_len: this operation is only applicable to Tuple \
         arguments"

let s_len (v : Val.t) : Val.t =
  match v with
  | Str s -> Int (String.length s)
  | _ ->
      invalid_arg
        "Exception in Oper.s_len: this operation is only applicable to String \
         arguments"

let s_len_u (v : Val.t) : Val.t =
  match v with
  | Str s -> Int (String_utils.s_len_u s)
  | _ ->
      invalid_arg
        "Exception in Oper.s_len_u: this operation is only applicable to \
         String arguments"

let list_nth ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | List l, Int i -> List.nth l i
  | _ ->
      invalid_arg
        "Exception in Oper.list_nth: this operation is only applicable to List \
         and Int arguments"

let array_nth ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Arr l, Int i -> Array.get l i
  | _ ->
      invalid_arg
        "Exception in Oper.array_nth: this operation is only applicable to \
         Array and Int arguments"

let tuple_nth ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Tuple l, Int i -> List.nth l i
  | _ ->
      invalid_arg
        "Exception in Oper.tuple_nth: this operation is only applicable to \
         Tuple and Int arguments"

let s_nth ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Str s, Int i -> Str (String.sub s i 1)
  | _ ->
      invalid_arg
        "Exception in Oper.s_nth: this operation is only applicable to String \
         and Integer arguments"

let s_nth_u ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Str s, Int i -> Str (String_utils.s_nth_u s i)
  | _ ->
      invalid_arg
        "Exception in Oper.s_nth_u: this operation is only applicable to \
         String and Integer arguments"

let s_substr ((v1, v2, v3) : Val.t * Val.t * Val.t) : Val.t =
  match (v1, v2, v3) with
  | Str s, Int i, Int j -> Str (String.sub s i j)
  | _ ->
      invalid_arg
        "Exception in Oper.s_substr: this operation is only applicable to \
         String and two Integer arguments"

let s_substr_u ((v1, v2, v3) : Val.t * Val.t * Val.t) : Val.t =
  match (v1, v2, v3) with
  | Str s, Int i, Int j -> Str (String_utils.s_substr_u s i j)
  | _ ->
      invalid_arg
        "Exception in Oper.s_substr_u: this operation is only applicable to \
         String and two Integer arguments"

(* TODO: i should be unicode index
   let re_exec (v1, v2, v3: Val.t * Val.t * Val.t) : Val.t = match v1, v2, v3 with
     | Str re, Str s, Int i ->
       let regex = Str.regexp re in
         let matched = Str.string_match regex s i in
           if matched then
             (Printf.printf "%s\n" (Str.matched_string s);
             Str (Str.matched_string s))
           else Str("")
     | _             -> invalid_arg "Exception in Oper.re_exec: this operation is only applicable to two String arguments and one Int argument"
*)

(**
 * JSON number regex: https://stackoverflow.com/a/13340826/3049315
 * Recognized Regexp constructs in OCaml Str: https://ocaml.org/api/Str.html
 *)
let parse_number (v : Val.t) : Val.t =
  match v with
  | Str s ->
      let regex =
        Str.regexp
          "-?\\(0\\|[1-9][0-9]*\\)\\(\\.[0-9]+\\)?\\([eE][+-]?[0-9]+\\)?"
      in
      let matched = Str.string_match regex s 0 in
      if matched then Str (Str.matched_string s) else Str ""
  | _ ->
      invalid_arg
        "Exception in Oper.parse_number: this operation is only applicable to \
         a String argument"

(**
 * JSON string regex: https://stackoverflow.com/a/32155765/3049315
 *)
let parse_string (v : Val.t) : Val.t =
  match v with
  | Str s ->
      let regex =
        Str.regexp
          "\"\\(\\\\\\([\"\\\\\\/bfnrt]\\|u[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)\\|[^\"\\\\\000-\031\127]+\\)*\""
      in
      let matched = Str.string_match regex s 0 in
      if matched then Str (Str.matched_string s) else Str ""
  | _ ->
      invalid_arg
        "Exception in Oper.parse_string: this operation is only applicable to \
         a String argument"

let parse_date (v : Val.t) : Val.t =
  match v with
  | Str s -> (
      let negative_year = s.[0] == '-' in
      Printf.printf "negative_year: %b\n" negative_year;
      if negative_year then
        let res =
          Date_utils.parse_date (String.sub s 1 (String.length s - 1))
        in
        match res with
        | None -> Val.Flt (-1.)
        | Some ([ year; month; day; hour; min; sec; msec ], tz) ->
            Val.List
              [
                Val.Flt (-.year);
                Val.Flt month;
                Val.Flt day;
                Val.Flt hour;
                Val.Flt min;
                Val.Flt sec;
                Val.Flt msec;
                Val.Str tz;
              ]
        | _ -> raise (Failure "Impossible: parse_date")
      else
        let positive_year = s.[0] == '+' in
        if positive_year then
          let res =
            Date_utils.parse_date (String.sub s 1 (String.length s - 1))
          in
          match res with
          | None -> Val.Flt (-1.)
          | Some ([ year; month; day; hour; min; sec; msec ], tz) ->
              Val.List
                [
                  Val.Flt year;
                  Val.Flt month;
                  Val.Flt day;
                  Val.Flt hour;
                  Val.Flt min;
                  Val.Flt sec;
                  Val.Flt msec;
                  Val.Str tz;
                ]
          | _ -> raise (Failure "Impossible: parse_date")
        else
          let res = Date_utils.parse_date s in
          match res with
          | None -> Val.Flt (-1.)
          | Some ([ year; month; day; hour; min; sec; msec ], tz) ->
              Val.List
                [
                  Val.Flt year;
                  Val.Flt month;
                  Val.Flt day;
                  Val.Flt hour;
                  Val.Flt min;
                  Val.Flt sec;
                  Val.Flt msec;
                  Val.Str tz;
                ]
          | _ -> raise (Failure "Impossible: parse_date"))
  | _ ->
      invalid_arg
        "Exception in Oper.parse_date: this operation is only applicable to a \
         String argument"

let list_in ((v1, v2) : Val.t * Val.t) : Val.t =
  match v2 with
  | List l -> Bool (List.mem v1 l)
  | _ ->
      invalid_arg
        "Exception in Oper.list_in: this operation is only applicable to List \
         arguments"

let list_add ((v1, v2) : Val.t * Val.t) : Val.t =
  match v1 with
  | List l -> Val.List (l @ [ v2 ])
  | _ ->
      invalid_arg
        "Exception in Oper.list_add: this operation is only applicable to List \
         arguments"

let list_prepend ((v1, v2) : Val.t * Val.t) : Val.t =
  (* Printf.printf "list_prepend: %s" (Val.str v2); *)
  match v2 with
  | List l -> Val.List (v1 :: l)
  | _ ->
      invalid_arg
        "Exception in Oper.list_prepend: this operation is only applicable to \
         a Value and a List arguments"

let list_concat ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | List l1, List l2 -> Val.List (l1 @ l2)
  | _ ->
      invalid_arg
        "Exception in Oper.list_concat: this operation is only applicable to \
         List arguments"

let list_reverse (v : Val.t) : Val.t =
  match v with
  | List l ->
      let rec rev_acc acc = function
        | [] -> acc
        | hd :: tl -> rev_acc (hd :: acc) tl
      in
      Val.List (rev_acc [] l)
  | _ ->
      invalid_arg
        "Exception in Oper.list_reverse: this operation is only applicable to \
         a List argument"

let head (v : Val.t) : Val.t =
  match v with
  | List l -> List.hd l
  | _ ->
      invalid_arg
        "Exception in Oper.head: this operation is only applicable to List \
         arguments"

let tail (v : Val.t) : Val.t =
  match v with
  | List l -> List (List.tl l)
  | _ ->
      invalid_arg
        "Exception in Oper.tail: this operation is only applicable to List \
         arguments"

let rec list_remove_aux l e =
  match l with
  | [] -> []
  | h :: tl -> if h = e then tl else h :: list_remove_aux tl e

let list_remove ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | List l, e -> List (list_remove_aux l e)
  | _ ->
      invalid_arg
        "Exception in Oper.list_remove: this operation is only applicable to \
         List and Any arguments"

let list_remove_last (v : Val.t) : Val.t =
  match v with
  | List l -> (
      let l' = List.rev l in
      match l' with _ :: l'' -> List (List.rev l'') | _ -> List [])
  | _ ->
      invalid_arg
        "Exception in Oper.list_remove_last: this operation is only applicable \
         to List arguments"

let rec list_remove_nth_aux ((v1, v2) : Val.t * Val.t) : Val.t list =
  match (v1, v2) with
  | List l, Int idx ->
      if idx = 0 then List.tl l
      else List.hd l :: list_remove_nth_aux (List (List.tl l), Int (idx - 1))
  | _ ->
      invalid_arg
        "Exception in Oper.list_remove_nth: this operation is only applicable \
         to List and Int arguments"

let list_remove_nth ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | List l, Int idx ->
      if idx >= 0 then List (list_remove_nth_aux (List l, Int idx))
      else
        invalid_arg
          "Exception in Oper.list_remove_nth: this operation is only \
           applicable to List and Int greater or equal to 0 arguments"
  | _ ->
      invalid_arg
        "Exception in Oper.list_remove_nth: this operation is only applicable \
         to List and Int arguments"

let list_sort (v : Val.t) : Val.t =
  match v with
  | List l -> (
      let strs =
        List.fold_left
          (fun acc v ->
            match (acc, v) with
            | Some strs, Val.Str s -> Some (strs @ [ s ])
            | _ -> None)
          (Some []) l
      in
      match strs with
      | None ->
          invalid_arg
            "Exception in Oper.list_sort: this operation is only applicable to \
             List of string arguments"
      | Some strs ->
          List
            (List.map (fun s -> Val.Str s) (List.fast_sort String.compare strs))
      )
  | _ ->
      invalid_arg
        "Exception in Oper.list_sort: this operation is only applicable to \
         List arguments"

let list_to_array (v1 : Val.t) : Val.t =
  match v1 with
  | List lst -> Val.Arr (Array.of_list lst)
  | _ ->
      invalid_arg
        "Exception in Oper.list_to_array: this operation is only applicable to \
         List arguments"

let array_make ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Int n, x -> Val.Arr (Array.make n x)
  | _ ->
      invalid_arg
        "Exception in Oper.array_make: this operation is only applicable to \
         Int and Value arguments"

let array_set ((v1, v2, v3) : Val.t * Val.t * Val.t) : Val.t =
  match (v1, v2, v3) with
  | Arr a, Int n, x ->
      Array.set a n x;
      Val.Null
  | _ ->
      invalid_arg
        "Exception in Oper.array_set: this operation is only applicable to \
         Array, Int and Value arguments"

let rec list_set_aux ((v1, v2, v3, v4) : Val.t * Val.t * Val.t * Val.t) :
    Val.t list =
  match (v1, v2, v3, v4) with
  | List l, Int idx, x, Int n ->
      if n = idx then x :: List.tl l
      else List.hd l :: list_set_aux (List (List.tl l), Int idx, x, Int (n + 1))
  | _ ->
      invalid_arg
        "Exception in Oper.list_set: this operation is only applicable to List \
         and Int arguments"

let list_set ((v1, v2, v3) : Val.t * Val.t * Val.t) : Val.t =
  match (v1, v2, v3) with
  | List l, Int idx, x ->
      if idx >= 0 && idx < List.length l then
        List (list_set_aux (List l, Int idx, x, Int 0))
      else
        invalid_arg
          "Exception in Oper.list_set: this operation is only applicable to \
           List, Int greater or equal to 0 and Any arguments"
  | _ ->
      invalid_arg
        "Exception in Oper.list_set: this operation is only applicable to \
         List, Int and Any arguments"

let ite ((v1, v2, v3) : Val.t * Val.t * Val.t) : Val.t =
  match v1 with Bool b -> if b then v2 else v3 | _ -> invalid_arg "something"

let first (v : Val.t) : Val.t =
  match v with
  | Tuple t -> List.hd t
  | _ ->
      invalid_arg
        "Exception in Oper.first: this operation is only applicable to Tuple \
         arguments"

let second (v : Val.t) : Val.t =
  match v with
  | Tuple t -> List.nth t 1
  | _ ->
      invalid_arg
        "Exception in Oper.second: this operation is only applicable to Tuple \
         arguments"

let int_to_string (v : Val.t) : Val.t =
  match v with
  | Int i -> Str (string_of_int i)
  | _ ->
      invalid_arg
        "Exception in Oper.int_to_string: this operation is only applicable to \
         Int arguments"

let int_to_float (v : Val.t) : Val.t =
  match v with
  | Int i -> Flt (float_of_int i)
  | _ ->
      invalid_arg
        "Exception in Oper.int_to_float: this operation is only applicable to \
         Int arguments"

let int_of_string (v : Val.t) : Val.t =
  match v with
  | Str s -> Int (int_of_string s)
  | _ ->
      invalid_arg
        "Exception in Oper.int_of_string: this operation is only applicable to \
         Str arguments"

let int_of_float (v : Val.t) : Val.t =
  match v with
  | Flt f -> Int (int_of_float f)
  | _ ->
      invalid_arg
        "Exception in Oper.int_of_float: this operation is only applicable to \
         Flt arguments."

let float_to_string (v : Val.t) : Val.t =
  match v with
  | Flt i -> Str (Arith_utils.float_to_string_inner i)
  | _ ->
      invalid_arg
        ("Exception in Oper.float_to_string: this operation is only applicable \
          to Flt arguments: " ^ Val.str v)

let float_of_string (v : Val.t) : Val.t =
  match v with
  | Str s -> (
      let trimmed = String.trim s in
      if String.length trimmed == 0 then Flt nan
      else try Flt (float_of_string trimmed) with _ -> Flt nan)
  | _ ->
      invalid_arg
        "Exception in Oper.float_of_string: this operation is only applicable \
         to Str arguments"

let string_concat (v : Val.t) : Val.t =
  match v with
  | List l -> (
      let strs =
        List.fold_left
          (fun acc v ->
            match (acc, v) with
            | Some strs, Val.Str s -> Some (strs @ [ s ])
            | _ -> None)
          (Some []) l
      in
      match strs with
      | None ->
          invalid_arg
            "Exception.Oper.string_concat: this operation is only applicable \
             to List of string arguments"
      | Some strs -> Str (String.concat "" strs))
  | _ ->
      invalid_arg
        "Exception in Oper.string_concat: this operation is only applicable to \
         List arguments"

(* Splits on character:
   let string_split (v, c : Val.t * Val.t) : Val.t = match v, c with
     | _, Str ""        -> invalid_arg "Exception in Oper.string_split: separator cannot be the empty string"
     | Str str, Str sep ->
       let c = String.get sep 0 in
       Val.List (List.map (fun str -> Val.Str str) (String.split_on_char c str))
     | _                -> invalid_arg "Exception in Oper.string_split: this operation is only applicable to String arguments"
*)
(* Splits on RegExp. Inspired by: https://stackoverflow.com/a/39814087/3049315 *)

let string_split ((v, c) : Val.t * Val.t) : Val.t =
  match (v, c) with
  | _, Str "" ->
      invalid_arg
        "Exception in Oper.string_split: separator cannot be the empty string"
  | Str str, Str sep ->
      Val.List
        (List.map (fun str -> Val.Str str) (Str.split (Str.regexp sep) str))
  | _ ->
      invalid_arg
        "Exception in Oper.string_split: this operation is only applicable to \
         String arguments"

let shift_left ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Flt (Arith_utils.int32_left_shift f1 f2)
  | _ ->
      invalid_arg
        "Exception in Oper.shift_left: this operation is only applicable to \
         Float arguments"

let shift_right ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Flt (Arith_utils.int32_right_shift f1 f2)
  | _ ->
      invalid_arg
        "Exception in Oper.shift_right: this operation is only applicable to \
         Float arguments"

let shift_right_logical ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Flt (Arith_utils.uint32_right_shift f1 f2)
  | _ ->
      invalid_arg
        "Exception in Oper.shift_right_logical: this operation is only \
         applicable to Float arguments"

let to_int (v : Val.t) : Val.t =
  match v with
  | Flt n -> Flt (Arith_utils.to_int n)
  | _ ->
      invalid_arg
        "Exception in Oper.to_int: this operation is only applicable to Float \
         arguments"

let to_int32 (v : Val.t) : Val.t =
  match v with
  | Flt n -> Flt (Arith_utils.to_int32 n)
  | _ ->
      invalid_arg
        "Exception in Oper.to_int32: this operation is only applicable to \
         Float arguments"

let to_uint32 (v : Val.t) : Val.t =
  match v with
  | Flt n -> Flt (Arith_utils.to_uint32 n)
  | _ ->
      invalid_arg
        "Exception in Oper.to_uint32: this operation is only applicable to \
         Float arguments"

let to_uint16 (v : Val.t) : Val.t =
  match v with
  | Flt n -> Flt (Arith_utils.to_uint16 n)
  | _ ->
      invalid_arg
        "Exception in Oper.to_uint16: this operation is only applicable to \
         Float arguments"

let log_2 (v : Val.t) : Val.t =
  match v with
  | Flt x -> Flt (Float.log x /. Float.log 2.)
  | _ ->
      invalid_arg
        "Exception in Oper.log_2: this operation is only applicable to Float \
         arguments"

let unpack_byte (v : Val.t) : int =
  match v with
  | Byte b -> b
  (*  | Byte64 b -> Int64.to_int32 b (* TODO temporário*) *)
  | _ ->
      invalid_arg
        "Exception in Oper.unpack_byte: this operation is only applicable to \
         Byte arguments"

(*
let unpack_byte64 (v : Val.t) : int64 = match v with
  | Byte64 b -> b
  | _ -> invalid_arg "Exception in Oper.unpack_byte64: this operation is only applicable to Byte64 arguments"
*)

let unpack_byte_to_str (v : Val.t) : string =
  match v with
  | Byte b -> string_of_int b
  | _ ->
      let msg =
        Printf.sprintf
          "Exception in unpack_byte_to_str: this opperation is only applicable \
           to Bypte arguments %s\n"
          (Val.str v)
      in
      invalid_arg msg

let bytes_to_string (v : Val.t) : Val.t =
  match v with
  (*| List bytes -> let bytes_string = "[" ^ (String.concat "; " (List.map unpack_byte_to_str bytes)) ^ "]" in*)
  | Arr bytes ->
      let bytes_string =
        "["
        ^ String.concat "; "
            (Array.to_list (Array.map unpack_byte_to_str bytes))
        ^ "]"
      in
      Str bytes_string
  | _ ->
      invalid_arg
        "Exception in Oper.bytes_to_string: this operation is only applicable \
         to Byte arguments"

let float_to_byte (v : Val.t) : Val.t =
  match v with
  | Flt x -> Val.Byte (Int64.to_int (Int64.bits_of_float x))
  | _ ->
      invalid_arg
        "Exception in Oper.float_to_byte: this operation is only applicable to \
         Float arguments"

let float64_to_le_bytes (v : Val.t) : Val.t =
  match v with
  | Flt x ->
      let bytes = Byte_utils.float64_to_le_bytes x in
      let val_bytes = List.map (fun b -> Val.Byte (Int64.to_int b)) bytes in
      List val_bytes
  | _ ->
      invalid_arg
        "Exception in Oper.float64_to_le_bytes: this operation is only \
         applicable to Float arguments"

let float64_to_be_bytes (v : Val.t) : Val.t =
  match v with
  | Flt x ->
      let bytes = Byte_utils.float64_to_be_bytes x in
      let val_bytes = List.map (fun b -> Val.Byte (Int64.to_int b)) bytes in
      List val_bytes
  | _ ->
      invalid_arg
        "Exception in Oper.float64_to_be_bytes: this operation is only \
         applicable to Float arguments"

let float32_to_le_bytes (v : Val.t) : Val.t =
  match v with
  | Flt x ->
      let bytes = Byte_utils.float32_to_le_bytes x in
      let val_bytes = List.map (fun b -> Val.Byte (Int32.to_int b)) bytes in
      List val_bytes
  | _ ->
      invalid_arg
        "Exception in Oper.float32_to_le_bytes: this operation is only \
         applicable to Float arguments"

let float32_to_be_bytes (v : Val.t) : Val.t =
  match v with
  | Flt x ->
      let bytes = Byte_utils.float32_to_be_bytes x in
      let val_bytes = List.map (fun b -> Val.Byte (Int32.to_int b)) bytes in
      List val_bytes
  | _ ->
      invalid_arg
        "Exception in Oper.float32_to_be_bytes: this operation is only \
         applicable to Float arguments"

let int_to_be_bytes ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Flt x, Int n ->
      let bytes = Byte_utils.int_to_be_bytes (x, n) in
      let val_bytes = List.map (fun b -> Val.Byte b) bytes in
      List val_bytes
  | _ ->
      invalid_arg
        "Exception in Oper.int_to_be_bytes: this operation is only applicable \
         to Float and Int arguments"

let float64_from_le_bytes (v : Val.t) : Val.t =
  match v with
  | Arr bytes ->
      let int_bytes = Array.map unpack_byte bytes in
      let int64_bytes = Array.map Int64.of_int int_bytes in
      let f = Byte_utils.float64_from_le_bytes int64_bytes in
      Flt f
  | _ ->
      invalid_arg
        "Exception in Oper.float64_from_le_bytes: this operation is only \
         applicable to List arguments"

let float64_from_be_bytes (v : Val.t) : Val.t =
  match v with
  | Arr bytes ->
      let int_bytes = Array.map unpack_byte bytes in
      let int64_bytes = Array.map Int64.of_int int_bytes in
      let f = Byte_utils.float64_from_be_bytes int64_bytes in
      Flt f
  | _ ->
      invalid_arg
        "Exception in Oper.float64_from_be_bytes: this operation is only \
         applicable to List arguments"

let float32_from_le_bytes (v : Val.t) : Val.t =
  match v with
  | Arr bytes ->
      let int_bytes = Array.map unpack_byte bytes in
      let int32_bytes = Array.map Int32.of_int int_bytes in
      let f = Byte_utils.float32_from_le_bytes int32_bytes in
      Flt f
  | _ ->
      invalid_arg
        "Exception in Oper.float32_from_le_bytes: this operation is only \
         applicable to Array arguments"

let float32_from_be_bytes (v : Val.t) : Val.t =
  match v with
  | Arr bytes ->
      let int_bytes = Array.map unpack_byte bytes in
      let int32_bytes = Array.map Int32.of_int int_bytes in
      let f = Byte_utils.float32_from_be_bytes int32_bytes in
      Flt f
  | _ ->
      invalid_arg
        "Exception in Oper.float64_from_le_bytes: this operation is only \
         applicable to Array arguments"

(*temporario ate juntar representações de bytes*)
let unpack_tmp (v : Val.t) : int =
  match v with
  | Int b | Byte b -> b
  | _ ->
      invalid_arg
        "Exception in Oper.unpack_tmp: this operation is only applicable to \
         Int arguments"

let int_from_le_bytes ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Arr bytes, Int n ->
      let arr_bytes = Array.map unpack_tmp bytes in
      let int = Byte_utils.int_from_le_bytes (arr_bytes, n) in
      Flt int
  | _ ->
      invalid_arg
        "Exception in Oper.int_from_le_bytes: this operation is only \
         applicable to Array and Int arguments"

let uint_from_le_bytes ((v1, v2) : Val.t * Val.t) : Val.t =
  match (v1, v2) with
  | Arr bytes, Int n ->
      let arr_bytes = Array.map unpack_tmp bytes in
      let uint = Byte_utils.uint_from_le_bytes (arr_bytes, n) in
      Flt uint
  | _ ->
      invalid_arg
        "Exception in Oper.uint_from_le_bytes: this operation is only \
         applicable to Array and Int arguments"

let from_char_code (v : Val.t) : Val.t =
  match v with
  | Int n -> Str (String_utils.from_char_code n)
  | _ ->
      invalid_arg
        "Exception in Oper.from_char_code: this operation is only applicable \
         to Int arguments"

let from_char_code_u (v : Val.t) : Val.t =
  match v with
  | Int n -> Str (String_utils.from_char_code_u n)
  | _ ->
      invalid_arg
        "Exception in Oper.from_char_code_u: this operation is only applicable \
         to Int arguments"

let to_char_code (v : Val.t) : Val.t =
  match v with
  | Str s -> Int (String_utils.to_char_code s)
  | _ ->
      invalid_arg
        "Exception in Oper.to_char_code: this operation is only applicable to \
         Str arguments"

let to_char_code_u (v : Val.t) : Val.t =
  match v with
  | Str s -> Int (String_utils.to_char_code_u s)
  | _ ->
      invalid_arg
        "Exception in Oper.to_char_code_u: this operation is only applicable \
         to Str arguments"

let int_to_four_hex (v : Val.t) : Val.t =
  match v with
  | Int i -> Str (Printf.sprintf "%04x" i)
  | _ ->
      invalid_arg
        "Exception in Oper.int_to_four_hex: this operation is only applicable \
         to Int arguments"

let utf8_decode (v : Val.t) : Val.t =
  match v with
  | Str s -> Str (String_utils.utf8decode s)
  | _ ->
      invalid_arg
        "Exception in Oper.utf8_decode: this operation is only applicable to \
         Str arguments"

let hex_decode (v : Val.t) : Val.t =
  match v with
  | Str s -> Str (String_utils.hexdecode s)
  | _ ->
      invalid_arg
        "Exception in Oper.hex_decode: this operation is only applicable to \
         Str arguments"

let octal_to_decimal (v : Val.t) : Val.t =
  match v with
  | Int o ->
      let rec loop dec_value base temp =
        if temp = 0 then dec_value
        else
          let dec_value = dec_value + (temp mod 10 * base) in
          loop dec_value (base * 8) (temp / 10)
      in
      Int (loop 0 1 o)
  | _ ->
      invalid_arg
        "Exception in Oper.octal_to_decimal: this operation is only applicable \
         to Int arguments"

let to_lower_case (v : Val.t) : Val.t =
  match v with
  | Str s -> Str (String_utils.to_lower_case s)
  | _ ->
      invalid_arg
        "Exception in Oper.to_lower_case: this operation is only applicable to \
         Str arguments"

let to_upper_case (v : Val.t) : Val.t =
  match v with
  | Str s -> Str (String_utils.to_upper_case s)
  | _ ->
      invalid_arg
        "Exception in Oper.to_upper_case: this operation is only applicable to \
         Str arguments"

let trim (v : Val.t) : Val.t =
  match v with
  | Str s -> Str (String_utils.trim s)
  | _ ->
      invalid_arg
        "Exception in Oper.trim: this operation is only applicable to Str \
         arguments"

let str_of_const (c : const) : string =
  match c with
  | MAX_VALUE -> "MAX_VALUE"
  | MIN_VALUE -> "MIN_VALUE"
  | PI -> "PI"

let str_of_unopt (op : uopt) : string =
  match op with
  | Neg -> "-"
  | Not -> "!"
  | IsNaN -> "is_NaN"
  | BitwiseNot -> "~"
  | Typeof -> "typeof"
  | ListLen -> "l_len"
  | TupleLen -> "t_len"
  | StringLen -> "s_len"
  | StringLenU -> "s_len_u"
  | Head -> "hd"
  | Tail -> "tl"
  | First -> "fst"
  | Second -> "snd"
  | LRemoveLast -> "l_remove_last"
  | LSort -> "l_sort"
  | LReverse -> "l_reverse"
  | IntToFloat -> "int_to_float"
  | IntToString -> "int_to_string"
  | IntToFourHex -> "int_to_four_hex"
  | IntOfString -> "int_of_string"
  | IntOfFloat -> "int_of_float"
  | FloatOfString -> "float_of_string"
  | FloatToString -> "float_to_string"
  | HexDecode -> "hex_decode"
  | Utf8Decode -> "utf8_decode"
  | OctalToDecimal -> "octal_to_decimal"
  | ObjToList -> "obj_to_list"
  | Sconcat -> "s_concat"
  | ObjFields -> "obj_fields"
  | ToInt -> "to_int"
  | ToInt32 -> "to_int32"
  | ToUint32 -> "to_uint32"
  | ToUint16 -> "to_uint16"
  | FromCharCode -> "from_char_code"
  | FromCharCodeU -> "from_char_code_u"
  | ToCharCode -> "to_char_code"
  | ToCharCodeU -> "to_char_code_u"
  | ToLowerCase -> "to_lower_case"
  | ToUpperCase -> "to_upper_case"
  | Trim -> "trim"
  | Abs -> "abs"
  | Acos -> "acos"
  | Asin -> "asin"
  | Atan -> "atan"
  | Ceil -> "ceil"
  | Cos -> "cos"
  | Exp -> "exp"
  | Floor -> "floor"
  | Log_e -> "log_e"
  | Log_10 -> "log_10"
  | Random -> "random"
  | Sin -> "sin"
  | Sqrt -> "sqrt"
  | Tan -> "tan"
  | ParseNumber -> "parse_number"
  | ParseString -> "parse_string"
  | ParseDate -> "parse_date"
  | Cosh -> "cosh"
  | Log_2 -> "log_2"
  | Sinh -> "sinh"
  | Tanh -> "tanh"
  | Float64ToLEBytes -> "float64_to_le_bytes"
  | Float64ToBEBytes -> "float64_to_be_bytes"
  | Float32ToLEBytes -> "float32_to_le_bytes"
  | Float32ToBEBytes -> "float32_to_be_bytes"
  | Float64FromLEBytes -> "float64_from_le_bytes"
  | Float64FromBEBytes -> "float64_from_be_bytes"
  | Float32FromLEBytes -> "float32_from_le_bytes"
  | Float32FromBEBytes -> "float32_from_be_bytes"
  | BytesToString -> "bytes_to_string"
  | FloatToByte -> "float_to_byte"
  | ArrayLen -> "a_len"
  | ListToArray -> "list_to_array"

let str_of_binopt_single (op : bopt) : string =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Modulo -> "%"
  | Eq -> "="
  | Gt -> ">"
  | Lt -> "<"
  | Ge -> ">="
  | Le -> "<="
  | Log_And -> "&&"
  | Log_Or -> "||"
  | BitwiseAnd -> "&"
  | BitwiseOr -> "|"
  | BitwiseXor -> "^"
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | ShiftRightLogical -> ">>>"
  | InObj -> "in_obj"
  | InList -> "in_list"
  | Lnth -> "l_nth"
  | LRem -> "l_remove"
  | LRemNth -> "l_remove_nth"
  | Tnth -> "t_nth"
  | Snth -> "s_nth"
  | Snth_u -> "s_nth_u"
  | Ssplit -> "s_split"
  | Ladd -> "l_add"
  | Lprepend -> "l_prepend"
  | Lconcat -> "l_concat"
  | Atan2 -> "atan2"
  | Max -> "max"
  | Min -> "min"
  | Pow -> "**"
  | ToPrecision -> "to_precision"
  | ToExponential -> "to_exponential"
  | ToFixed -> "to_fixed"
  | ArrayMake -> "array_make"
  | Anth -> "a_nth"
  | IntToBEBytes -> "int_to_be_bytes"
  | IntFromBytes -> "int_from_le_bytes"
  | UintFromBytes -> "uint_from_le_bytes"

let str_of_binopt (op : bopt) (e1 : string) (e2 : string) : string =
  match op with
  | Plus -> e1 ^ " + " ^ e2
  | Minus -> e1 ^ " - " ^ e2
  | Times -> e1 ^ " * " ^ e2
  | Div -> e1 ^ " / " ^ e2
  | Modulo -> e1 ^ " % " ^ e2
  | Eq -> e1 ^ " = " ^ e2
  | Gt -> e1 ^ " > " ^ e2
  | Lt -> e1 ^ " < " ^ e2
  | Ge -> e1 ^ " >= " ^ e2
  | Le -> e1 ^ " <= " ^ e2
  | Log_And -> e1 ^ " && " ^ e2
  | Log_Or -> e1 ^ " || " ^ e2
  | BitwiseAnd -> e1 ^ " & " ^ e2
  | BitwiseOr -> e1 ^ " | " ^ e2
  | BitwiseXor -> e1 ^ " ^ " ^ e2
  | ShiftLeft -> e1 ^ " << " ^ e2
  | ShiftRight -> e1 ^ " >> " ^ e2
  | ShiftRightLogical -> e1 ^ " >>> " ^ e2
  | InObj -> e1 ^ " in_obj " ^ e2
  | InList -> e1 ^ " in_list " ^ e2
  | Lnth -> "l_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | LRem -> "l_remove(" ^ e1 ^ ", " ^ e2 ^ ")"
  | LRemNth -> "l_remove_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Tnth -> "t_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Snth -> "s_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Snth_u -> "s_nth_u(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Ssplit -> Printf.sprintf "s_split(%s, %s)" e1 e2
  | Ladd -> "l_add(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Lprepend -> "l_prepend(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Lconcat -> "l_concat(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Atan2 -> "atan2(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Max -> "max(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Min -> "min(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Pow -> e1 ^ " ** " ^ e2
  | ToPrecision -> "to_precision(" ^ e1 ^ ", " ^ e2 ^ ")"
  | ToExponential -> "to_exponential(" ^ e1 ^ ", " ^ e2 ^ ")"
  | ToFixed -> "to_fixed(" ^ e1 ^ ", " ^ e2 ^ ")"
  | ArrayMake -> "array_make(" ^ e1 ^ ", " ^ e2 ^ ")"
  | Anth -> "a_nth(" ^ e1 ^ ", " ^ e2 ^ ")"
  | IntToBEBytes -> "int_to_be_bytes(" ^ e1 ^ ", " ^ e2 ^ ")"
  | IntFromBytes -> "int_from_le_bytes(" ^ e1 ^ ", " ^ e2 ^ ")"
  | UintFromBytes -> "uint_from_le_bytes(" ^ e1 ^ ", " ^ e2 ^ ")"

let str_of_triopt_single (op : topt) : string =
  match op with
  | Ssubstr -> "s_substr"
  | SsubstrU -> "s_substr_u"
  | Aset -> "a_set"
  | Lset -> "l_set"
  | ITE -> "ite"

let str_of_triopt (op : topt) (e1 : string) (e2 : string) (e3 : string) : string
    =
  match op with
  | Ssubstr -> "s_substr(" ^ e1 ^ ", " ^ e2 ^ ", " ^ e3 ^ ")"
  | SsubstrU -> "s_substr_u(" ^ e1 ^ ", " ^ e2 ^ ", " ^ e3 ^ ")"
  | Aset -> "a_set(" ^ e1 ^ ", " ^ e2 ^ ", " ^ e3 ^ ")"
  | Lset -> "l_set(" ^ e1 ^ ", " ^ e2 ^ ", " ^ e3 ^ ")"
  | ITE -> "ite(" ^ e1 ^ ", " ^ e2 ^ ", " ^ e3 ^ ")"

let str_of_nopt (op : nopt) (es : string list) : string =
  match op with
  | ListExpr -> "[ " ^ String.concat ", " es ^ " ]"
  | TupleExpr -> "( " ^ String.concat ", " es ^ " )"
  | NAry_And -> String.concat " && " es
  | NAry_Or -> String.concat " || " es
  | ArrExpr -> "[| " ^ String.concat ", " es ^ " |]"

let unary_float_call (func : float -> float) (v : Val.t) (failure_msg : string)
    : Val.t =
  match v with
  | Flt f -> Flt (func f)
  | _ ->
      invalid_arg
        (Printf.sprintf "Exception in %s: expected float, got %s" failure_msg
           (Val.str v))

let binary_float_call (func : float -> float -> float) (v1 : Val.t) (v2 : Val.t)
    (failure_msg : string) : Val.t =
  match (v1, v2) with
  | Flt f1, Flt f2 -> Flt (func f1 f2)
  | _ ->
      invalid_arg
        (Printf.sprintf "Exception in %s: expected floats, got %s and %s"
           failure_msg (Val.str v1) (Val.str v2))

let apply_uopt_oper (oper : uopt) (v : Val.t) : Val.t =
  match oper with
  | Abs -> unary_float_call Float.abs v "Absolute value"
  | Acos -> unary_float_call Float.acos v "Arc cosine"
  | Asin -> unary_float_call Float.asin v "Arc sine"
  | Atan -> unary_float_call Float.atan v "Arc tangent"
  | Ceil -> unary_float_call Float.ceil v "Ceil"
  | Cos -> unary_float_call Float.cos v "Cosine"
  | Exp -> unary_float_call Float.exp v "Exponential"
  | Floor -> unary_float_call Float.floor v "Floor"
  | Log_e -> unary_float_call Float.log v "Natural logarithm"
  | Log_10 -> unary_float_call Float.log10 v "Base-10 logarithm"
  | Random -> unary_float_call Random.float v "Random"
  | Sin -> unary_float_call Float.sin v "Sine"
  | Sqrt -> unary_float_call Float.sqrt v "Square root"
  | Tan -> unary_float_call Float.tan v "Tangent"
  | Cosh -> unary_float_call Float.cosh v "Cosh"
  | Sinh -> unary_float_call Float.sinh v "Sinh"
  | Tanh -> unary_float_call Float.tanh v "Tanh"
  | _ ->
      invalid_arg
        ("Exception in Oper.apply_uopt_oper: unexpected unary operator: "
       ^ str_of_unopt oper)

let apply_bopt_oper (oper : bopt) (v1 : Val.t) (v2 : Val.t) : Val.t =
  match oper with
  | Atan2 -> binary_float_call Float.atan2 v1 v2 "Arc tangent of quotient y/x"
  | Max -> binary_float_call Float.max v1 v2 "Max"
  | Min -> binary_float_call Float.min v1 v2 "Min"
  | Pow -> binary_float_call Float.pow v1 v2 "Power"
  | _ ->
      invalid_arg
        ("Exception in Oper.apply_bopt_oper: unexpected binary operator: "
        ^ str_of_binopt oper (Val.str v1) (Val.str v2))

let bopt_to_json (op : bopt) : string =
  Printf.sprintf "{ \"type\" : \"binopt\", \"value\" : \"%s"
    (match op with
    | Plus -> Printf.sprintf "Plus\" }"
    | Minus -> Printf.sprintf "Minus\" }"
    | Times -> Printf.sprintf "Times\" }"
    | Div -> Printf.sprintf "Div\" }"
    | Modulo -> Printf.sprintf "Modulo\" }"
    | Eq -> Printf.sprintf "Equal\" }"
    | Gt -> Printf.sprintf "Gt\" }"
    | Lt -> Printf.sprintf "Lt\" }"
    | Ge -> Printf.sprintf "Egt\" }"
    | Le -> Printf.sprintf "Elt\" }"
    | Log_And -> Printf.sprintf "Log_And\" }"
    | Log_Or -> Printf.sprintf "Log_Or\" }"
    | BitwiseAnd -> Printf.sprintf "BitwiseAnd\" }"
    | BitwiseOr -> Printf.sprintf "BitwiseOr\" }"
    | BitwiseXor -> Printf.sprintf "BitwiseXor\" }"
    | ShiftLeft -> Printf.sprintf "ShiftLeft\" }"
    | ShiftRight -> Printf.sprintf "ShiftRight\" }"
    | ShiftRightLogical -> Printf.sprintf "ShiftRightLogical\" }"
    | InObj -> Printf.sprintf "InObj\" }"
    | InList -> Printf.sprintf "InList\" }"
    | Lnth -> Printf.sprintf "Lnth\" }"
    | LRem -> Printf.sprintf "LRem\" }"
    | LRemNth -> Printf.sprintf "LRemNth\" }"
    | Tnth -> Printf.sprintf "Tnth\" }"
    | Snth -> Printf.sprintf "Snth\" }"
    | Snth_u -> Printf.sprintf "Snth_u\" }"
    | Ssplit -> Printf.sprintf "Ssplit\" }"
    | Ladd -> Printf.sprintf "Ladd\" }"
    | Lprepend -> Printf.sprintf "Lprepend\" }"
    | Lconcat -> Printf.sprintf "Lconcat\" }"
    | Atan2 -> Printf.sprintf "Atan2\" }"
    | Max -> Printf.sprintf "Max\" }"
    | Min -> Printf.sprintf "Min\" }"
    | Pow -> Printf.sprintf "Pow\" }"
    | ToPrecision -> Printf.sprintf "To_Precision\" }"
    | ToExponential -> Printf.sprintf "To_Exponential\" }"
    | ToFixed -> Printf.sprintf "To_Fixed\" }"
    | ArrayMake -> Printf.sprintf "Array_Make\" }"
    | Anth -> Printf.sprintf "Anth\" }"
    | IntToBEBytes -> Printf.sprintf "IntToBEBytes\" }"
    | IntFromBytes -> Printf.sprintf "IntFromBytes\" }"
    | UintFromBytes -> Printf.sprintf "UintFromBytes\" }")

let topt_to_json (op : topt) : string =
  Printf.sprintf "{ \"type\" : \"triopt\", \"value\" : \"%s"
    (match op with
    | Ssubstr -> Printf.sprintf "Ssubstr\" }"
    | SsubstrU -> Printf.sprintf "SsubstrU\" }"
    | Aset -> Printf.sprintf "Aset\" }"
    | Lset -> Printf.sprintf "Lset\" }"
    | ITE -> Printf.sprintf "ITE\" }")

let nopt_to_json (op : nopt) : string =
  Printf.sprintf "{ \"type\" : \"nopt\", \"value\" : \"%s"
    (match op with
    | ListExpr -> Printf.sprintf "ListExpr\" }"
    | TupleExpr -> Printf.sprintf "TupleExpr\" }"
    | NAry_And -> Printf.sprintf "NAry_And\" }"
    | NAry_Or -> Printf.sprintf "NAry_Or\" }"
    | ArrExpr -> Printf.sprintf "ArrExpr\" }")

let uopt_to_json (op : uopt) : string =
  Printf.sprintf "{ \"type\" : \"unopt\", \"value\" : \"%s"
    (match op with
    | Neg -> Printf.sprintf "Neg\" }"
    | Not -> Printf.sprintf "Not\" }"
    | IsNaN -> Printf.sprintf "IsNaN\" }"
    | BitwiseNot -> Printf.sprintf "BitwiseNot\" }"
    | Typeof -> Printf.sprintf "Typeof\" }"
    | ListLen -> Printf.sprintf "ListLen\" }"
    | TupleLen -> Printf.sprintf "TypleLen\" }"
    | StringLen -> Printf.sprintf "StringLen\" }"
    | StringLenU -> Printf.sprintf "StringLenU\" }"
    | Head -> Printf.sprintf "Head\" }"
    | Tail -> Printf.sprintf "Tail\" }"
    | First -> Printf.sprintf "First\" }"
    | Second -> Printf.sprintf "Second\" }"
    | LRemoveLast -> Printf.sprintf "LRemoveLast\" }"
    | LSort -> Printf.sprintf "LSort\" }"
    | LReverse -> Printf.sprintf "LReverse\" }"
    | IntToFloat -> Printf.sprintf "IntToFloat\" }"
    | IntToString -> Printf.sprintf "IntToString\" }"
    | IntToFourHex -> Printf.sprintf "IntToFourHex\" }"
    | IntOfString -> Printf.sprintf "IntOfString\" }"
    | IntOfFloat -> Printf.sprintf "IntOfFloat\" }"
    | FloatOfString -> Printf.sprintf "FloatOfString\" }"
    | FloatToString -> Printf.sprintf "FloatToString\" }"
    | HexDecode -> Printf.sprintf "HexDecode\" }"
    | Utf8Decode -> Printf.sprintf "Utf8Decode\" }"
    | OctalToDecimal -> Printf.sprintf "OctalToDecimal\" }"
    | ObjToList -> Printf.sprintf "ObjToList\" }"
    | Sconcat -> Printf.sprintf "Sconcat\" }"
    | ObjFields -> Printf.sprintf "ObjFields\" }"
    | ToInt -> Printf.sprintf "ToInt\" }"
    | ToInt32 -> Printf.sprintf "ToInt32\" }"
    | ToUint32 -> Printf.sprintf "ToUint32\" }"
    | ToUint16 -> Printf.sprintf "ToUint16\" }"
    | FromCharCode -> Printf.sprintf "FromCharCode\" }"
    | FromCharCodeU -> Printf.sprintf "FromCharCodeU\" }"
    | ToCharCode -> Printf.sprintf "ToCharCode\" }"
    | ToCharCodeU -> Printf.sprintf "ToCharCodeU\" }"
    | ToLowerCase -> Printf.sprintf "ToLowerCase\" }"
    | ToUpperCase -> Printf.sprintf "ToUpperCase\" }"
    | Trim -> Printf.sprintf "Trim\" }"
    | Abs -> Printf.sprintf "Abs\" }"
    | Acos -> Printf.sprintf "Acos\" }"
    | Asin -> Printf.sprintf "Asin\" }"
    | Atan -> Printf.sprintf "Atan\" }"
    | Ceil -> Printf.sprintf "Ceil\" }"
    | Cos -> Printf.sprintf "Cos\" }"
    | Exp -> Printf.sprintf "Exp\" }"
    | Floor -> Printf.sprintf "Floor\" }"
    | Log_e -> Printf.sprintf "Log_e\" }"
    | Log_10 -> Printf.sprintf "Log_10\" }"
    | Random -> Printf.sprintf "Random\" }"
    | Sin -> Printf.sprintf "Sin\" }"
    | Sqrt -> Printf.sprintf "Sqrt\" }"
    | Tan -> Printf.sprintf "Tan\" }"
    | ParseNumber -> Printf.sprintf "ParseNumber\" }"
    | ParseString -> Printf.sprintf "ParseString\" }"
    | ParseDate -> Printf.sprintf "ParseDate\" }"
    | Cosh -> Printf.sprintf "Cosh\" }"
    | Log_2 -> Printf.sprintf "Log2\" }"
    | Sinh -> Printf.sprintf "Sinh\" }"
    | Tanh -> Printf.sprintf "Tanh\" }"
    | Float64ToLEBytes -> Printf.sprintf "Float64ToLEBytes\" }"
    | Float64ToBEBytes -> Printf.sprintf "Float64ToBEBytes\" }"
    | Float32ToLEBytes -> Printf.sprintf "Float32ToLEBytes\" }"
    | Float32ToBEBytes -> Printf.sprintf "Float32ToBEBytes\" }"
    | Float64FromLEBytes -> Printf.sprintf "Float64FromLEBytes\" }"
    | Float64FromBEBytes -> Printf.sprintf "Float64FromBEBytes\" }"
    | Float32FromLEBytes -> Printf.sprintf "Float32FromLEBytes\" }"
    | Float32FromBEBytes -> Printf.sprintf "Float32FromBEBytes\" }"
    | BytesToString -> Printf.sprintf "BytesToString\" }"
    | FloatToByte -> Printf.sprintf "FloatToByte\" }"
    | ArrayLen -> Printf.sprintf "ArrayLen\" }"
    | ListToArray -> Printf.sprintf "ListToArray\" }")

open Smtml
open EslBase
open EslSyntax.Operator

let op_err (arg : int) (op_lbl : string) (rterr : Runtime_error.msg) : 'a =
  try Runtime_error.(throw ~src:(Index arg) rterr)
  with Runtime_error.Error err ->
    Runtime_error.(push (OpEvalErr op_lbl) err |> raise)

let unexpected_err (arg : int) (op_lbl : string) (msg : string) : 'a =
  op_err arg op_lbl (Unexpected msg)

let bad_arg_err (arg : int) (op_lbl : string) (types : string)
  (vals : Smtml.Value.t list) : 'a =
  op_err arg op_lbl (BadOpArgs (types, vals))

let typeof (_v : Value.t) : Value.t =
  (* TODO:x *)
  assert false

  (* let op_lbl = label_of_unopt Typeof in
  match v with
  | Null -> Type NullType
  | Void -> unexpected_err 1 op_lbl "void value"
  | Int _ -> Type IntType
  | Flt _ -> Type RealType
  | Bool _ -> Type BoolType
  | Str _ -> Type StrType
  | Symbol _ -> Type SymbolType
  | Loc _ -> Type LocType
  | Arr _ -> Type ArrayType
  | List _ -> Type ListType
  | Tuple _ -> Type TupleType
  | Type _ -> Type TypeType
  | Byte _ -> Log.fail "'typeof(byte)' not implemented"
  | Curry _ -> Type CurryType *)

let neg (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt Neg in
  match v with
  | Int v -> Int (-v)
  | Real v -> Real (-.v)
  | _ -> bad_arg_err 1 op_lbl "integer or float" [ v ]

let bitwise_not (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt BitwiseNot in
  match v with
  | Real f -> Real (Arith_utils.int32_bitwise_not f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let logical_not (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt LogicalNot in
  match v with
  | Value.True -> Value.False
  | Value.False -> Value.True
  | _ -> bad_arg_err 1 op_lbl "boolean" [ v ]

let int_to_float (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt IntToFloat in
  match v with
  | Int i -> Real (float_of_int i)
  | _ -> bad_arg_err 1 op_lbl "integer" [ v ]

let int_to_string (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt IntToString in
  match v with
  | Int i -> Str (string_of_int i)
  | _ -> bad_arg_err 1 op_lbl "integer" [ v ]

let float_to_int (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt FloatToInt in
  match v with
  | Real f -> Int (int_of_float f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let float_to_string (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt FloatToString in
  match v with
  | Real i -> Str (Arith_utils.float_to_string_inner i)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let string_to_int (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt StringToInt in
  match v with
  | Str s -> Int (int_of_string s)
  | _ -> bad_arg_err 1 op_lbl "string" [ v ]

let string_to_float (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt StringToFloat in
  match v with
  | Str s -> (
    let trimmed = String.trim s in
    if String.length trimmed == 0 then Real nan
    else try Real (float_of_string trimmed) with _ -> Real nan )
  | _ -> bad_arg_err 1 op_lbl "string" [ v ]

let from_char_code (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt FromCharCode in
  match v with
  | Int n -> Str (String_utils.from_char_code n)
  | _ -> bad_arg_err 1 op_lbl "integer" [ v ]

let to_char_code (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt ToCharCode in
  match v with
  | Str s -> Int (String_utils.to_char_code s)
  | _ -> bad_arg_err 1 op_lbl "string" [ v ]

let string_len (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt StringLen in
  match v with
  | Str s -> Int (String.length s)
  | _ -> bad_arg_err 1 op_lbl "string" [ v ]

let string_concat_aux (lst : Value.t list) : string list option =
  let concat_f acc v =
    match (acc, v) with
    | (Some strs, Value.Str s) -> Some (strs @ [ s ])
    | _ -> None
  in
  List.fold_left concat_f (Some []) lst

let string_concat (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt StringConcat in
  match v with
  | List lst -> (
    let strs = string_concat_aux lst in
    match strs with
    | Some strs -> Str (String.concat "" strs)
    | None -> bad_arg_err 1 op_lbl "string list" [ v ] )
  | _ -> bad_arg_err 1 op_lbl "string list" [ v ]

let list_head (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt ListHead in
  match v with
  | List (hd :: _) -> hd
  | _ -> bad_arg_err 1 op_lbl "non-empty list" [ v ]

let list_tail (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt ListTail in
  match v with
  | List (_ :: tl) -> List tl
  | _ -> bad_arg_err 1 op_lbl "list" [ v ]

let list_len (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt ListLen in
  match v with
  | List lst -> Value.Int (List.length lst)
  | _ -> bad_arg_err 1 op_lbl "list" [ v ]

let list_reverse (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt ListReverse in
  match v with
  | List lst -> Value.List (List.rev lst)
  | _ -> bad_arg_err 1 op_lbl "list" [ v ]

let tuple_first (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt TupleFirst in
  match v with
  | List tup -> List.nth tup 0
  | _ -> bad_arg_err 1 op_lbl "tuple" [ v ]

let tuple_second (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt TupleSecond in
  match v with
  | List tup -> List.nth tup 1
  | _ -> bad_arg_err 1 op_lbl "tuple" [ v ]

let tuple_len (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt TupleLen in
  match v with
  | List tup -> Value.Int (List.length tup)
  | _ -> bad_arg_err 1 op_lbl "tuple" [ v ]

let random (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt Random in
  match v with
  | Real f -> Real (Random.float f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let abs (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt Abs in
  match v with
  | Real f -> Real (Float.abs f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let sqrt (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt Sqrt in
  match v with
  | Real f -> Real (Float.sqrt f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let ceil (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt Ceil in
  match v with
  | Real f -> Real (Float.ceil f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let floor (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt Floor in
  match v with
  | Real f -> Real (Float.floor f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let trunc (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt Trunc in
  match v with
  | Real f -> Real (Float.trunc f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let exp (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt Exp in
  match v with
  | Real f -> Real (Float.exp f)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let plus ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt Plus in
  match (v1, v2) with
  | (Int i1, Int i2) -> Int (i1 + i2)
  | (Real f1, Real f2) -> Real (f1 +. f2)
  | (Str s1, Str s2) -> Str (s1 ^ s2)
  | ((Int _ | Real _ | Str _), _) ->
    bad_arg_err 2 op_lbl
      "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
  | _ ->
    bad_arg_err 1 op_lbl
      "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]

let minus ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt Minus in
  match (v1, v2) with
  | (Int i1, Int i2) -> Int (i1 - i2)
  | (Real f1, Real f2) -> Real (f1 -. f2)
  | (Int _, _) | (Real _, _) ->
    bad_arg_err 2 op_lbl "(integer, integer) or (float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(integer, integer) or (float, float)" [ v1; v2 ]

let times ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt Times in
  match (v1, v2) with
  | (Int i1, Int i2) -> Int (i1 * i2)
  | (Real f1, Real f2) -> Real (f1 *. f2)
  | (Int _, _) | (Real _, _) ->
    bad_arg_err 2 op_lbl "(integer, integer) or (float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(integer, integer) or (float, float)" [ v1; v2 ]

let div ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt Div in
  match (v1, v2) with
  | (Int i1, Int i2) -> Int (i1 / i2)
  | (Real f1, Real f2) -> Real (f1 /. f2)
  | (Int _, _) | (Real _, _) ->
    bad_arg_err 2 op_lbl "(integer, integer) or (float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(integer, integer) or (float, float)" [ v1; v2 ]

let modulo ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt Modulo in
  match (v1, v2) with
  | (Real f1, Real f2) -> Real (mod_float f1 f2)
  | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

let pow ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt Pow in
  match (v1, v2) with
  | (Real f1, Real f2) -> Real (Float.pow f1 f2)
  | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

let bitwise_and ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt BitwiseAnd in
  match (v1, v2) with
  | (Real f1, Real f2) -> Real (Arith_utils.int32_bitwise_and f1 f2)
  | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

let bitwise_or ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt BitwiseOr in
  match (v1, v2) with
  | (Real f1, Real f2) -> Real (Arith_utils.int32_bitwise_or f1 f2)
  | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

let bitwise_xor ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt BitwiseXor in
  match (v1, v2) with
  | (Real f1, Real f2) -> Real (Arith_utils.int32_bitwise_xor f1 f2)
  | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

let shift_left ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt ShiftLeft in
  match (v1, v2) with
  | (Real f1, Real f2) -> Real (Arith_utils.int32_left_shift f1 f2)
  | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

let shift_right ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt ShiftRight in
  match (v1, v2) with
  | (Real f1, Real f2) -> Real (Arith_utils.int32_right_shift f1 f2)
  | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

let shift_right_logical ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt ShiftRightLogical in
  match (v1, v2) with
  | (Real f1, Real f2) -> Real (Arith_utils.uint32_right_shift f1 f2)
  | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

let logical_and ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt LogicalAnd in
  match (v1, v2) with
  | (Value.True, Value.False) | (Value.False, Value.True) | (Value.False, Value.False) ->
    Value.False
  | (Value.True, Value.True) -> Value.True
  | ((Value.True | Value.False), _) -> bad_arg_err 2 op_lbl "(boolean, boolean)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(boolean, boolean)" [ v1; v2 ]

let logical_or ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt LogicalOr in
  match (v1, v2) with
  | (Value.True, Value.True) | (Value.True, Value.False) | (Value.False, Value.True) -> Value.True
  | (Value.False, Value.False) -> Value.False
  | ((Value.True | Value.False), _) -> bad_arg_err 2 op_lbl "(boolean, boolean)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(boolean, boolean)" [ v1; v2 ]

let mk_bool (v : bool) : Value.t = if v then Value.True else Value.False

let eq ((v1, v2) : Value.t * Value.t) : Value.t = mk_bool (Value.equal v1 v2) 
let ne ((v1, v2) : Value.t * Value.t) : Value.t = mk_bool (not @@ Value.equal v1 v2) 

(* TODO: This should be defined using Val.compare *)
let lt ((v1, v2) : Value.t * Value.t) : Value.t =
  match (v1, v2) with
  | (Real f, Int i) -> mk_bool (f < float i)
  | (Int i, Real f) -> mk_bool (float i < f)
  | (v1, v2) -> mk_bool (v1 < v2)

(* TODO: This should be defined using Val.compare *)
let gt ((v1, v2) : Value.t * Value.t) : Value.t =
  match (v1, v2) with
  | (Real f, Int i) -> mk_bool (f > float i)
  | (Int i, Real f) -> mk_bool (float i > f)
  | (v1, v2) -> mk_bool (v1 > v2)

(* TODO: This should be defined using Val.compare *)
let le ((v1, v2) : Value.t * Value.t) : Value.t =
  match (v1, v2) with
  | (Real f, Int i) -> mk_bool (f <= float i)
  | (Int i, Real f) -> mk_bool (float i <= f)
  | (v1, v2) -> mk_bool (v1 <= v2)

(* TODO: This should be defined using Val.compare *)
let ge ((v1, v2) : Value.t * Value.t) : Value.t =
  match (v1, v2) with
  | (Real f, Int i) -> mk_bool (f >= float i)
  | (Int i, Real f) -> mk_bool (float i >= f)
  | (v1, v2) -> mk_bool (v1 >= v2)

let string_nth ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt StringNth in
  match (v1, v2) with
  | (Str s, Int i) -> (
    try Str (String.sub s i 1)
    with _ -> unexpected_err 2 op_lbl "index out of bounds" )
  | (Str _, _) -> bad_arg_err 2 op_lbl "(string, integer)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(string, integer)" [ v1; v2 ]

let list_nth ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt ListNth in
  match (v1, v2) with
  | (List lst, Int i) -> (
    try List.nth lst i with _ -> unexpected_err 2 op_lbl "index out of bounds" )
  | (List _, _) -> bad_arg_err 2 op_lbl "(list, integer)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(list, integer)" [ v1; v2 ]

let list_add ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt ListAdd in
  match v1 with
  | List lst -> List (lst @ [ v2 ])
  | _ -> bad_arg_err 1 op_lbl "(list, any)" [ v1; v2 ]

let list_prepend ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt ListPrepend in
  match v2 with
  | List lst -> List (v1 :: lst)
  | _ -> bad_arg_err 2 op_lbl "(any, list)" [ v1; v2 ]

let list_concat ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt ListPrepend in
  match (v1, v2) with
  | (List l1, List l2) -> List (l1 @ l2)
  | (List _, _) -> bad_arg_err 2 op_lbl "(list, list)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(list, list)" [ v1; v2 ]

let tuple_nth ((v1, v2) : Value.t * Value.t) : Value.t =
  let op_lbl = label_of_binopt TupleNth in
  match (v1, v2) with
  | (List tup, Int i) -> (
    try List.nth tup i with _ -> unexpected_err 2 op_lbl "index out of bounds" )
  | (List _, _) -> bad_arg_err 2 op_lbl "(tuple, integer)" [ v1; v2 ]
  | _ -> bad_arg_err 1 op_lbl "(tuple, integer)" [ v1; v2 ]

let ite ((v1, v2, v3) : Value.t * Value.t * Value.t) : Value.t =
  let op_lbl = label_of_triopt ITE in
  match v1 with
  | Value.True -> v2
  | Value.False -> v3
  | _ -> bad_arg_err 1 op_lbl "(boolean, any, any)" [ v1; v2; v3 ]

let s_substr ((v1, v2, v3) : Value.t * Value.t * Value.t) : Value.t =
  let op_lbl = label_of_triopt StringSubstr in
  let err_msg = "(string, integer, integer)" in
  let arg_err i = bad_arg_err i op_lbl err_msg [ v1; v2; v3 ] in
  match (v1, v2, v3) with
  | (Str s, Int i, Int j) -> Str (String.sub s i j)
  | (Str _, Int _, _) -> arg_err 3
  | (Str _, _, _) -> arg_err 2
  | _ -> arg_err 1

let list_set ((v1, v2, v3) : Value.t * Value.t * Value.t) : Value.t =
  let op_lbl = label_of_triopt ListSet in
  let rec _set_aux lst i v =
    match (lst, i) with
    | ([], _) -> v :: unexpected_err 2 op_lbl "index out of bounds"
    | (_ :: tl, 0) -> v :: tl
    | (hd :: tl, _) -> hd :: _set_aux tl (i - 1) v
  in
  match (v1, v2) with
  | (List lst, Int i) -> List (_set_aux lst i v3)
  | (List _, _) -> bad_arg_err 2 op_lbl "(list, integer, any)" [ v1; v2; v3 ]
  | _ -> bad_arg_err 1 op_lbl "(list, integer, any)" [ v1; v2; v3 ]

let to_bool_aux op_lbl (vals : Value.t list) (v : Value.t) : bool =
  match v with 
  | Value.True -> true 
  | Value.False -> false 
  | _ -> bad_arg_err 1 op_lbl "boolean list" vals

let nary_logical_and (vals : Value.t list) : Value.t =
  let op_lbl = label_of_nopt NAryLogicalAnd in
  mk_bool (List.for_all (to_bool_aux op_lbl vals) vals)

let nary_logical_or (vals : Value.t list) : Value.t =
  let op_lbl = label_of_nopt NAryLogicalOr in
  mk_bool (List.exists (to_bool_aux op_lbl vals) vals)

let array_expr (_vals : Value.t list) : Value.t =(*  Arr (Array.of_list vals) *) failwith "array_expr"
let list_expr (vals : Value.t list) : Value.t = List vals
let tuple_expr (vals : Value.t list) : Value.t = List vals

let eval_unopt (op : unopt) (v : Value.t) : Value.t =
  match op with
  | Typeof -> typeof v
  | Neg -> neg v
  | BitwiseNot -> bitwise_not v
  | LogicalNot -> logical_not v
  | IntToFloat -> int_to_float v
  | IntToString -> int_to_string v
  | FloatToInt -> float_to_int v
  | FloatToString -> float_to_string v
  | StringToInt -> string_to_int v
  | StringToFloat -> string_to_float v
  | FromCharCode -> from_char_code v
  | ToCharCode -> to_char_code v
  | StringLen -> string_len v
  | StringConcat -> string_concat v
  | ObjectToList -> Log.fail "unexpected 'ObjectToList' operator"
  | ObjectFields -> Log.fail "unexpected 'ObjectFields' operator"
  | ListHead -> list_head v
  | ListTail -> list_tail v
  | ListLen -> list_len v
  | ListReverse -> list_reverse v
  | TupleFirst -> tuple_first v
  | TupleSecond -> tuple_second v
  | TupleLen -> tuple_len v
  | Random -> random v
  | Abs -> abs v
  | Sqrt -> sqrt v
  | Ceil -> ceil v
  | Floor -> floor v
  | Trunc -> trunc v
  | Exp -> exp v

let eval_binopt (op : binopt) (v1 : Value.t) (v2 : Value.t) : Value.t =
  match op with
  | Plus -> plus (v1, v2)
  | Minus -> minus (v1, v2)
  | Times -> times (v1, v2)
  | Div -> div (v1, v2)
  | Modulo -> modulo (v1, v2)
  | Pow -> pow (v1, v2)
  | BitwiseAnd -> bitwise_and (v1, v2)
  | BitwiseOr -> bitwise_or (v1, v2)
  | BitwiseXor -> bitwise_xor (v1, v2)
  | ShiftLeft -> shift_left (v1, v2)
  | ShiftRight -> shift_right (v1, v2)
  | ShiftRightLogical -> shift_right_logical (v1, v2)
  | LogicalAnd -> logical_and (v1, v2)
  | LogicalOr -> logical_or (v1, v2)
  | SCLogicalAnd -> Log.fail "unexpected 'SCLogicalAnd' operator"
  | SCLogicalOr -> Log.fail "unexpected 'SCLogicalOr' operator"
  | Eq -> eq (v1, v2)
  | NE -> ne (v1, v2)
  | Lt -> lt (v1, v2)
  | Gt -> gt (v1, v2)
  | Le -> le (v1, v2)
  | Ge -> ge (v1, v2)
  | ObjectMem -> Log.fail "unexpected 'ObjectMem' operator"
  | StringNth -> string_nth (v1, v2)
  | ListNth -> list_nth (v1, v2)
  | ListAdd -> list_add (v1, v2)
  | ListPrepend -> list_prepend (v1, v2)
  | ListConcat -> list_concat (v1, v2)
  | TupleNth -> tuple_nth (v1, v2)

let eval_triopt (op : triopt) (v1 : Value.t) (v2 : Value.t) (v3 : Value.t) : Value.t =
  match op with
  | ITE -> ite (v1, v2, v3)
  | StringSubstr -> s_substr (v1, v2, v3)
  | ListSet -> list_set (v1, v2, v3)

let eval_nopt (op : nopt) (vals : Value.t list) : Value.t =
  match op with
  | NAryLogicalAnd -> nary_logical_and vals
  | NAryLogicalOr -> nary_logical_or vals
  | ArrayExpr -> (* Val.Arr (Array.of_list vals) *) failwith "array_expr"
  | ListExpr -> List vals
  | TupleExpr -> List vals

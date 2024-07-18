open Smtml
open EslBase
open EslSyntax
open EslSyntax.Operator

let op_err (arg : int) (op_lbl : string) (rterr : Runtime_error.msg) : 'a =
  try Runtime_error.(throw ~src:(Index arg) rterr)
  with Runtime_error.Error err ->
    Runtime_error.(push (OpEvalErr op_lbl) err |> raise)

let unexpected_err (arg : int) (op_lbl : string) (msg : string) : 'a =
  op_err arg op_lbl (Unexpected msg)

let bad_arg_err (arg : int) (op_lbl : string) (types : string)
  (vals : Value.t list) : 'a =
  op_err arg op_lbl (BadOpArgs (types, vals))

let op_error index value ty op =
  let open Ty in
  match (op, ty) with
  (* unop *)
  | (`Unop Not, Ty_int) ->
    bad_arg_err index (label_of_unopt BitwiseNot) "integer" [ value ]
  | (`Unop Not, Ty_bool) ->
    bad_arg_err index (label_of_unopt LogicalNot) "boolean" [ value ]
  | (`Cvtop Reinterpret_int, _) ->
    bad_arg_err index (label_of_unopt IntToFloat) "integer" [ value ]
  | (`Cvtop String_from_int, _) ->
    bad_arg_err index (label_of_unopt IntToString) "integer" [ value ]
  | (`Cvtop Reinterpret_float, _) ->
    bad_arg_err index (label_of_unopt FloatToInt) "float" [ value ]
  | (`Cvtop String_to_int, _) ->
    bad_arg_err index (label_of_unopt StringToInt) "string" [ value ]
  | (`Cvtop String_from_code, _) ->
    bad_arg_err index (label_of_unopt FromCharCode) "integer" [ value ]
  | (`Cvtop String_to_code, _) ->
    bad_arg_err index (label_of_unopt ToCharCode) "string" [ value ]
  | (`Unop Length, Ty_str) ->
    bad_arg_err index (label_of_unopt StringLen) "string" [ value ]
  | (`Unop Head, _) ->
    bad_arg_err index (label_of_unopt ListHead) "non-empty list" [ value ]
  | (`Unop Tail, _) ->
    bad_arg_err index (label_of_unopt ListTail) "list" [ value ]
  | (`Unop Length, Ty_list) ->
    bad_arg_err index (label_of_unopt ListLen) "list" [ value ]
  | (`Unop Reverse, _) ->
    bad_arg_err index (label_of_unopt ListReverse) "list" [ value ]
  | (`Unop Abs, _) -> bad_arg_err index (label_of_unopt Abs) "float" [ value ]
  | (`Unop Sqrt, _) -> bad_arg_err index (label_of_unopt Sqrt) "float" [ value ]
  | (`Unop Ceil, _) -> bad_arg_err index (label_of_unopt Ceil) "float" [ value ]
  | (`Unop Floor, _) ->
    bad_arg_err index (label_of_unopt Floor) "float" [ value ]
  | (`Unop Trunc, _) ->
    bad_arg_err index (label_of_unopt Trunc) "float" [ value ]
  (* binop *)
  | (`Binop Rem, _) ->
    bad_arg_err index (label_of_binopt Modulo) "(float, float)" [ value ]
  | (`Binop Pow, _) ->
    bad_arg_err index (label_of_binopt Pow) "(float, float)" [ value ]
  | (`Binop And, Ty_int) ->
    bad_arg_err index
      (label_of_binopt BitwiseAnd)
      "(integer, integer)" [ value ]
  | (`Binop Or, Ty_int) ->
    bad_arg_err index (label_of_binopt BitwiseOr) "(integer, integer)" [ value ]
  | (`Binop Xor, Ty_int) ->
    bad_arg_err index
      (label_of_binopt BitwiseXor)
      "(integer, integer)" [ value ]
  | (`Binop Shl, Ty_int) ->
    bad_arg_err index (label_of_binopt ShiftLeft) "(integer, integer)" [ value ]
  | (`Binop ShrA, Ty_int) ->
    bad_arg_err index
      (label_of_binopt ShiftRight)
      "(integer, integer)" [ value ]
  | (`Binop ShrL, Ty_int) ->
    bad_arg_err index
      (label_of_binopt ShiftRightLogical)
      "(integer, integer)" [ value ]
  | (`Binop And, Ty_bool) ->
    bad_arg_err index
      (label_of_binopt LogicalAnd)
      "(boolean, boolean)" [ value ]
  | (`Binop Or, Ty_bool) ->
    bad_arg_err index (label_of_binopt LogicalOr) "(boolean, boolean)" [ value ]
  | (`Binop List_append_last, _) ->
    bad_arg_err index (label_of_binopt ListAdd) "(list, any)" [ value ]
  (* triop *)
  | (`Triop Ite, _) ->
    bad_arg_err index (label_of_triopt ITE) "(boolean, any, any)" [ value ]
  | (`Triop String_extract, _) ->
    bad_arg_err index
      (label_of_triopt StringSubstr)
      "(string, integer, integer)" [ value ]
  | (`Triop List_set, _) ->
    bad_arg_err index (label_of_triopt ListSet) "(list, integer, any)" [ value ]
  (* other errors *)
  | (`Unop op', _) ->
    unexpected_err index
      (Format.asprintf "%a" pp_unop op')
      (Format.asprintf "unop with type %a" pp ty)
  | (`Binop op', _) ->
    unexpected_err index
      (Format.asprintf "%a" pp_binop op')
      (Format.asprintf "binop with type %a" pp ty)
  | (`Relop op', _) ->
    unexpected_err index
      (Format.asprintf "%a" pp_relop op')
      (Format.asprintf "relop with type %a" pp ty)
  | (`Triop op', _) ->
    unexpected_err index
      (Format.asprintf "%a" pp_triop op')
      (Format.asprintf "triop with type %a" pp ty)
  | (`Cvtop op', _) ->
    unexpected_err index
      (Format.asprintf "%a" pp_cvtop op')
      (Format.asprintf "cvtop with type %a" pp ty)
  | (`Naryop op', _) ->
    unexpected_err index
      (Format.asprintf "%a" pp_naryop op')
      (Format.asprintf "naryop with type %a %d" pp ty index)

let float_to_string (v : Value.t) : Value.t =
  let op_lbl = label_of_unopt FloatToString in
  match v with
  | Real i -> Str (Arith_utils.float_to_string_inner i)
  | _ -> bad_arg_err 1 op_lbl "float" [ v ]

let unop_semantics (op : Operator.unopt) : Value.t -> Value.t =
  match op with
  | Neg -> (
    function
    | Value.Int _ as v -> Eval.(unop Ty_int Ty.Neg) v
    | Value.Real _ as v -> Eval.(unop Ty_real Ty.Neg) v
    | _ as v -> bad_arg_err 1 (label_of_unopt Neg) "integer or float" [ v ] )
  | BitwiseNot -> Eval.(unop Ty_int Ty.Not)
  | LogicalNot -> Eval.(unop Ty_bool Ty.Not)
  | IntToFloat -> Eval.(cvtop Ty_int Ty.Reinterpret_int)
  | IntToString -> Eval.(cvtop Ty_str Ty.String_from_int)
  | FloatToInt -> Eval.(cvtop Ty_real Ty.Reinterpret_float)
  | FloatToString -> float_to_string
  | StringToInt -> Eval.(cvtop Ty_str Ty.String_to_int)
  | StringToFloat -> (
    function
    | Value.Str _ as v -> (
      try Eval.(cvtop Ty_str Ty.String_to_float) v with _ -> Real nan )
    | _ as v -> bad_arg_err 1 (label_of_unopt StringToFloat) "string" [ v ] )
  | FromCharCode -> Eval.(cvtop Ty_str Ty.String_from_code)
  | ToCharCode -> Eval.(cvtop Ty_str Ty.String_to_code)
  | StringLen -> Eval.(unop Ty_str Ty.Length)
  | ObjectToList -> Log.fail "unexpected 'ObjectToList' operator"
  | ObjectFields -> Log.fail "unexpected 'ObjectFields' operator"
  | StringConcat -> (
    function
    | Value.List lst -> Eval.(naryop Ty_str Ty.Concat) lst
    | _ as v -> bad_arg_err 1 (label_of_unopt StringConcat) "string list" [ v ]
    )
  | ListHead -> Eval.(unop Ty_list Ty.Head)
  | ListTail -> Eval.(unop Ty_list Ty.Tail)
  | ListLen -> Eval.(unop Ty_list Ty.Length)
  | ListReverse -> Eval.(unop Ty_list Ty.Reverse)
  | Abs -> Eval.(unop Ty_real Ty.Abs)
  | Sqrt -> Eval.(unop Ty_real Ty.Sqrt)
  | Ceil -> Eval.(unop Ty_real Ty.Ceil)
  | Floor -> Eval.(unop Ty_real Ty.Floor)
  | Trunc -> Eval.(unop Ty_real Ty.Trunc)

let binop_semantics (op : Operator.binopt) =
  let make_bool b = if b then Value.True else Value.False in
  match op with
  | Plus -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Int _, Value.Int _) -> Eval.(binop Ty_int Ty.Add v1 v2)
      | (Value.Real _, Value.Real _) -> Eval.(binop Ty_real Ty.Add v1 v2)
      | (Value.Str _, Value.Str _) -> Eval.(naryop Ty_str Ty.Concat [ v1; v2 ])
      | ((Int _ | Real _ | Str _), _) ->
        bad_arg_err 2 (label_of_binopt Plus)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt Plus)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
    )
  | Minus -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Int _, Value.Int _) -> Eval.(binop Ty_int Ty.Sub v1 v2)
      | (Value.Real _, Value.Real _) -> Eval.(binop Ty_real Ty.Sub v1 v2)
      | (Int _, _) | (Real _, _) ->
        bad_arg_err 2 (label_of_binopt Minus)
          "(integer, integer) or (float, float)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt Minus)
          "(integer, integer) or (float, float)" [ v1; v2 ] )
  | Times -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Int _, Value.Int _) -> Eval.(binop Ty_int Ty.Mul v1 v2)
      | (Value.Real _, Value.Real _) -> Eval.(binop Ty_real Ty.Mul v1 v2)
      | (Int _, _) | (Real _, _) ->
        bad_arg_err 2 (label_of_binopt Times)
          "(integer, integer) or (float, float)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt Times)
          "(integer, integer) or (float, float)" [ v1; v2 ] )
  | Div -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Int _, Value.Int _) -> Eval.(binop Ty_int Ty.Div v1 v2)
      | (Value.Real _, Value.Real _) -> Eval.(binop Ty_real Ty.Div v1 v2)
      | (Int _, _) | (Real _, _) ->
        bad_arg_err 2 (label_of_binopt Div)
          "(integer, integer) or (float, float)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt Div)
          "(integer, integer) or (float, float)" [ v1; v2 ] )
  | Modulo -> Eval.(binop Ty_real Ty.Rem)
  | Pow -> Eval.(binop Ty_real Ty.Pow)
  | BitwiseAnd -> Eval.(binop Ty_int Ty.And)
  | BitwiseOr -> Eval.(binop Ty_int Ty.Or)
  | BitwiseXor -> Eval.(binop Ty_int Ty.Xor)
  | ShiftLeft -> Eval.(binop Ty_int Ty.Shl)
  | ShiftRight -> Eval.(binop Ty_int Ty.ShrA)
  | ShiftRightLogical -> Eval.(binop Ty_int Ty.ShrL)
  | LogicalAnd -> Eval.(binop Ty_bool Ty.And)
  | LogicalOr -> Eval.(binop Ty_bool Ty.Or)
  | SCLogicalAnd -> Log.fail "unexpected 'SCLogicalAnd' operator"
  | SCLogicalOr -> Log.fail "unexpected 'SCLogicalOr' operator"
  | Eq -> fun v1 v2 -> make_bool (Value.equal v1 v2)
  | Ne -> fun v1 v2 -> make_bool (not @@ Value.equal v1 v2)
  | Lt -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Int _, Value.Int _) -> make_bool Eval.(relop Ty_int Ty.Lt v1 v2)
      | (Value.Real _, Value.Real _) ->
        make_bool Eval.(relop Ty_real Ty.Lt v1 v2)
      | (Value.Str _, Value.Str _) -> make_bool Eval.(relop Ty_str Ty.Lt v1 v2)
      | (Int _, _) | (Real _, _) ->
        bad_arg_err 2 (label_of_binopt Lt)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt Lt)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
    )
  | Le -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Int _, Value.Int _) -> make_bool Eval.(relop Ty_int Ty.Le v1 v2)
      | (Value.Real _, Value.Real _) ->
        make_bool Eval.(relop Ty_real Ty.Le v1 v2)
      | (Value.Str _, Value.Str _) -> make_bool Eval.(relop Ty_str Ty.Le v1 v2)
      | (Int _, _) | (Real _, _) ->
        bad_arg_err 2 (label_of_binopt Le)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt Le)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
    )
  | Gt -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Int _, Value.Int _) -> make_bool Eval.(relop Ty_int Ty.Gt v1 v2)
      | (Value.Real _, Value.Real _) ->
        make_bool Eval.(relop Ty_real Ty.Gt v1 v2)
      | (Value.Str _, Value.Str _) -> make_bool Eval.(relop Ty_str Ty.Gt v1 v2)
      | (Int _, _) | (Real _, _) ->
        bad_arg_err 2 (label_of_binopt Gt)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt Gt)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
    )
  | Ge -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Int _, Value.Int _) -> make_bool Eval.(relop Ty_int Ty.Ge v1 v2)
      | (Value.Real _, Value.Real _) ->
        make_bool Eval.(relop Ty_real Ty.Ge v1 v2)
      | (Value.Str _, Value.Str _) -> make_bool Eval.(relop Ty_str Ty.Ge v1 v2)
      | (Int _, _) | (Real _, _) ->
        bad_arg_err 2 (label_of_binopt Ge)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt Ge)
          "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
    )
  | ObjectMem -> Log.fail "unexpected 'ObjectMem' operator"
  | StringNth -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.Str _, Value.Int _) -> (
        try Eval.(binop Ty_str Ty.At) v1 v2
        with _ ->
          unexpected_err 2 (label_of_binopt StringNth) "index out of bounds" )
      | (Str _, _) ->
        bad_arg_err 2 (label_of_binopt StringNth) "(string, integer)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt StringNth) "(string, integer)" [ v1; v2 ]
    )
  | ListNth -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.List _, Value.Int _) -> (
        try Eval.(binop Ty_list Ty.At) v1 v2
        with _ ->
          unexpected_err 2 (label_of_binopt ListNth) "index out of bounds" )
      | (Str _, _) ->
        bad_arg_err 2 (label_of_binopt ListNth) "(list, integer)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt ListNth) "(list, integer)" [ v1; v2 ] )
  | ListAdd -> Eval.(binop Ty_list Ty.List_append_last)
  | ListPrepend -> (
    fun v1 v2 ->
      match v2 with
      | Value.List _ -> Eval.(binop Ty_list Ty.List_append) v2 v1
      | _ ->
        bad_arg_err 1 (label_of_binopt ListPrepend) "(any, list)" [ v1; v2 ] )
  | ListConcat -> (
    fun v1 v2 ->
      match (v1, v2) with
      | (Value.List _, Value.List _) ->
        Eval.(naryop Ty_list Ty.Concat) [ v1; v2 ]
      | (List _, _) ->
        bad_arg_err 2 (label_of_binopt ListConcat) "(list, list)" [ v1; v2 ]
      | _ ->
        bad_arg_err 1 (label_of_binopt ListConcat) "(list, list)" [ v1; v2 ] )

let triop_semantics (op : Operator.triopt) =
  match op with
  | ITE -> Eval.triop Ty_bool Ty.Ite
  | StringSubstr -> Eval.(triop Ty_str Ty.String_extract)
  | ListSet -> Eval.(triop Ty_list Ty.List_set)

let to_bool_aux (v : Value.t) : bool =
  match v with
  | Value.True -> true
  | Value.False -> false
  | _ -> Log.fail "Eval_op.to_bool_aux"

let nary_logical_and (vals : Value.t list) : Value.t =
  if List.for_all to_bool_aux vals then Value.True else Value.False

let nary_logical_or (vals : Value.t list) : Value.t =
  if List.exists to_bool_aux vals then Value.True else Value.False

let eval_nopt (op : nopt) (vals : Value.t list) : Value.t =
  match op with
  | NAryLogicalAnd -> nary_logical_and vals
  | NAryLogicalOr -> nary_logical_or vals
  | ListExpr -> Value.List vals

open Smtml
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

  let bad_arg_err (arg : int) (op_lbl : string) (types : string)
  (vals : Value.t list) : 'a =
  op_err arg op_lbl (BadOpArgs (types, vals))

let unop_semantics (op : Operator.unopt) : Value.t -> Value.t = 
  try
  match op with
  | Neg -> ( function
            | Value.Int _ as v  -> Eval.(unop Ty_int Ty.Neg) v
            | Value.Real _ as v -> Eval.(unop Ty_real Ty.Neg) v
            | _ as v -> bad_arg_err 1 "Neg" "integer or float" [ v ])
  | BitwiseNot -> Eval.(unop Ty_int Ty.Not)
  | LogicalNot -> Eval.(unop Ty_bool Ty.Not)
  | IntToFloat -> Eval.(cvtop Ty_int Ty.Reinterpret_int)
  | IntToString -> Eval.(cvtop Ty_str Ty.String_from_int)
  | FloatToInt -> Eval.(cvtop Ty_real Ty.Reinterpret_float)
  | FloatToString -> Eval.(cvtop Ty_real Ty.ToString)
  | StringToInt -> Eval.(cvtop Ty_str Ty.String_to_int)
  | StringToFloat -> Eval.(cvtop Ty_str Ty.String_to_float)
  | FromCharCode -> Eval.(cvtop Ty_str Ty.String_from_code)
  | ToCharCode -> Eval.(cvtop Ty_str Ty.String_to_code)
  | StringLen -> Eval.(unop Ty_str Ty.Length)
  | ObjectToList -> failwith "unop_semantics.objectToList"
  | ObjectFields -> failwith "unop_semantics.objectFields"
  | StringConcat -> (* TODO:x Eval.(naryop Ty_str Ty.Concat) *) failwith "unop_semantics.objectFields"
  | ListHead -> Eval.(unop Ty_list Ty.Head)
  | ListTail -> Eval.(unop Ty_list Ty.Tail)
  | ListLen -> Eval.(unop Ty_list Ty.Length)
  | ListReverse -> Eval.(unop Ty_list Ty.Reverse)
  | Abs -> Eval.(unop Ty_real Ty.Abs)
  | Sqrt -> Eval.(unop Ty_real Ty.Sqrt)
  | Ceil -> Eval.(unop Ty_real Ty.Ceil)
  | Floor -> Eval.(unop Ty_real Ty.Floor)
  | Trunc -> Eval.(unop Ty_real Ty.Trunc)
  with
  | Smtml.Eval.TypeError {index; value; ty; _} -> Format.printf "index: %a, value: %a, ty: %a" Fmt.int index Value.pp value Ty.pp ty;
    failwith "unop_semantics"
  
let binop_semantics (op : Operator.binopt) =
  let make_bool b = if b then Value.True else Value.False in
  try
  match op with
  | Plus -> (fun v1 v2 -> match v1, v2 with 
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Add v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Add v1 v2)
            | Value.Str _ , Value.Str _  -> Eval.(naryop Ty_str Ty.Concat [v1; v2])
            | ((Int _ | Real _ | Str _), _) -> bad_arg_err 2 "binop_semantics.plus"
            "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.plus"
            "(integer, integer) or (float, float) or (string, string)" [ v1; v2 ])
  | Minus -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Sub v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Sub v1 v2)
            | (Int _, _) | (Real _, _) ->
              bad_arg_err 2 "binop_semantics.minus" "(integer, integer) or (float, float)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.minus" "(integer, integer) or (float, float)" [ v1; v2 ])
  | Times -> (fun v1 v2 -> match v1, v2 with 
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Mul v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Mul v1 v2)
            | (Int _, _) | (Real _, _) ->
              bad_arg_err 2 "binop_semantics.times" "(integer, integer) or (float, float)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.times" "(integer, integer) or (float, float)" [ v1; v2 ])
  | Div -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> Eval.(binop Ty_int Ty.Div v1 v2)
            | Value.Real _ , Value.Real _  -> Eval.(binop Ty_real Ty.Div v1 v2)
            | (Int _, _) | (Real _, _) ->
              bad_arg_err 2 "binop_semantics.div" "(integer, integer) or (float, float)" [ v1; v2 ]
            | _ -> bad_arg_err 1 "binop_semantics.div" "(integer, integer) or (float, float)" [ v1; v2 ])
  | Modulo -> Eval.(binop Ty_real Ty.Rem)
  | Pow -> Eval.(binop Ty_real Ty.Pow)
  (* FIXME: need do change to Ty_int after smtml has implemented*)
  | BitwiseAnd -> Eval.(binop (Ty_bitv 32) Ty.And)
  | BitwiseOr -> Eval.(binop (Ty_bitv 32) Ty.Or)
  | BitwiseXor -> Eval.(binop (Ty_bitv 32) Ty.Xor)
  | ShiftLeft -> Eval.(binop (Ty_bitv 32) Ty.Shl)
  | ShiftRight -> Eval.(binop (Ty_bitv 32) Ty.ShrA)
  | ShiftRightLogical -> Eval.(binop (Ty_bitv 32) Ty.ShrL)
  | LogicalAnd -> Eval.(binop Ty_bool Ty.And)
  | LogicalOr -> Eval.(binop Ty_bool Ty.Or)
  | SCLogicalAnd -> failwith "binop_semantics.scLogicalAnd"
  | SCLogicalOr -> failwith "binop_semantics.scLogicalOr"
  | Eq -> ( fun v1 v2 -> make_bool (not @@ Value.equal v1 v2))
  | NE -> ( fun v1 v2 -> make_bool (not @@ Value.equal v1 v2))
  | Lt -> ( fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> make_bool Eval.(relop Ty_int Ty.Lt v1 v2)
            | Value.Real _ , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Lt v1 v2)
            | _ -> failwith "binop_semantics.lt")
  | Le -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> make_bool Eval.(relop Ty_int Ty.Le v1 v2) 
            | Value.Real _ , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Le v1 v2)
            | _ -> failwith "binop_semantics.le")
  | Gt -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> make_bool Eval.(relop Ty_int Ty.Gt v1 v2)
            | Value.Real _ , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Gt v1 v2)
            | _ -> failwith "binop_semantics.gt")
  | Ge -> (fun v1 v2 -> match v1, v2 with
            | Value.Int _ , Value.Int _  -> make_bool Eval.(relop Ty_int Ty.Ge v1 v2)
            | Value.Real _ , Value.Real _  -> make_bool Eval.(relop Ty_real Ty.Ge v1 v2)
            | _ -> failwith "binop_semantics.ge")
  | ObjectMem -> failwith "binop_semantics.objectMem"
  (* TODO:x index out of bounds *)
  | StringNth -> Eval.(binop Ty_str Ty.At)
  | ListNth -> Eval.(binop Ty_list Ty.At) 
  | ListAdd -> Eval.(binop Ty_list Ty.List_append_last)
  | ListPrepend -> Eval.(binop Ty_list Ty.List_append)
  | ListConcat -> (* TOOD:x Eval.(naryop Ty_list Ty.Concat) *) failwith "binop_semantics.listConcat"
  with
| Smtml.Eval.TypeError {index; value; ty; _} -> Format.printf "index: %a, value: %a, ty: %a" Fmt.int index Value.pp value Ty.pp ty;
  failwith "binop_semantics"

let triop_semantics (op: Operator.triopt) =
  try 
  match op with
  | ITE -> Eval.triop Ty_bool Ty.Ite
  | StringSubstr -> Eval.(triop Ty_str Ty.String_extract)
  | ListSet -> Eval.(triop Ty_list Ty.List_set)
with
| Smtml.Eval.TypeError {index; value; ty; _} -> Format.printf "index: %a, value: %a, ty: %a" Fmt.int index Value.pp value Ty.pp ty;
  failwith "binop_semantics"
let to_bool_aux (v : Value.t) : bool =
  match v with 
  | Value.True -> true
  | Value.False -> false
  | _ -> failwith "Eval_op.to_bool_aux"

let nary_logical_and (vals : Value.t list) : Value.t =
  if (List.for_all to_bool_aux vals) then
    Value.True
  else 
    Value.False

let nary_logical_or (vals : Value.t list) : Value.t =
  if (List.exists to_bool_aux vals) then 
    Value.True
  else 
    Value.False

let eval_nopt (op : nopt) (vals : Value.t list) : Value.t =
  match op with
  | NAryLogicalAnd -> nary_logical_and vals
  | NAryLogicalOr -> nary_logical_or vals
  | ArrayExpr -> (* TODO:x Value.Arr (Array.of_list vals) *) failwith "eval_nopt.arrayExpr"
  | ListExpr -> Value.List vals
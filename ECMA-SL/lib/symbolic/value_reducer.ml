open Core
open Operators
open Val
module Value = Sym_value.M
open Value

let reduce_sconcat (vs : value list) : value =
  let s =
    List.fold_left vs ~init:[] ~f:(fun acc v ->
        match acc with
        | [] -> [ v ]
        | h :: t -> (
            match (h, v) with
            | Val (Str h'), Val (Str v') ->
                Val (Str (String.concat ~sep:"" [ h'; v' ])) :: t
            | Val (Str ""), _ -> v :: t
            | _, Val (Str "") -> acc
            | Val (Str _), _ -> v :: acc
            | _, Val (Str _) -> v :: acc
            | Symbolic (Type.StrType, _), Symbolic (Type.StrType, _) -> v :: acc
            | _ -> v :: acc))
  in
  let s = List.rev s in
  match s with
  | [] -> Val (Str "")
  | [ v ] -> v
  | _ -> UnOpt (Sconcat, NOpt (ListExpr, s))

let reduce_list_compare (list1 : value list) (list2 : value list) : value =
  if List.length list1 = List.length list2 then
    let comp val1 val2 =
      match (val1, val2) with
      | Symbolic (_tp1, n1), Symbolic (_tp2, n2) -> BinOpt (Eq, n1, n2)
      | Symbolic (_, n), Val _ -> BinOpt (Eq, n, val2)
      | Val _, Symbolic (_, n) -> BinOpt (Eq, val1, n)
      | Val val1, Val val2 -> Val (Bool (Val.equal val1 val2))
      | _ -> failwith "incompatible type for eq"
    in
    let concat curr v =
      match (curr, v) with
      | _, BinOpt (_, _, _) -> BinOpt (Log_And, curr, v)
      | BinOpt (_, _, _), _ -> BinOpt (Log_And, curr, v)
      | Val (Bool b1), Val (Bool b2) ->
          Val (Operators.log_and (Bool b1, Bool b2))
      | _ -> failwith "wrong"
    in
    let rec fold_until l1 l2 acc =
      match (l1, l2) with
      | [], [] -> acc
      | h1 :: t1, h2 :: t2 -> (
          let res = comp h1 h2 in
          match res with
          | Val (Bool false) -> res
          | Val (Bool true) | BinOpt (_, _, _) ->
              fold_until t1 t2 (concat acc res)
          | _ -> failwith "impossible comparison result")
      | _ -> failwith "lists must have same length"
    in

    fold_until list1 list2 (Val (Bool true))
  else Val (Bool false)

let reduce_list_set (list : value list) (idx : int) (newVal : value) : value =
  let rec list_set_aux ((v1, v2, v3, v4) : value list * Val.t * value * Val.t) :
      value list =
    match (v1, v2, v3, v4) with
    | head :: tail, Int idx, x, Int n ->
        if n = idx then x :: tail
        else head :: list_set_aux (tail, Int idx, x, Int (n + 1))
    | _ ->
        invalid_arg
          "Exception in Oper.list_set: this operation is only applicable to \
           List and Int arguments"
  in

  if idx >= 0 && idx < List.length list then
    NOpt (ListExpr, list_set_aux (list, Int idx, newVal, Int 0))
  else
    invalid_arg
      "Exception in Oper.list_set: this operation is only applicable to List, \
       Int greater or equal to 0 and Any arguments"

let reduce_unop (op : uopt) (v : value) : value =
  match (op, v) with
  | op, Val v -> Val (Eval_op.eval_unop op v)
  | Neg, Symbolic (_, _) -> UnOpt (Neg, v)
  | IsNaN, Symbolic _ -> Val (Bool false)
  | Not, _v' -> UnOpt (Not, v)
  | Head, NOpt (ListExpr, l) -> List.hd_exn l
  | Tail, NOpt (ListExpr, _ :: tl) -> NOpt (ListExpr, tl)
  | First, NOpt (TupleExpr, l) -> List.hd_exn l
  | Second, NOpt (TupleExpr, _ :: b :: _) -> b
  | ListLen, NOpt (ListExpr, vs) -> Val (Int (List.length vs))
  | ListLen, UnOpt (LSort, NOpt (ListExpr, vs)) -> Val (Int (List.length vs))
  | ListLen, UnOpt (LSort, lst) -> UnOpt (ListLen, lst)
  | TupleLen, NOpt (TupleExpr, vs) -> Val (Int (List.length vs))
  | LSort, NOpt (ListExpr, []) -> NOpt (ListExpr, [])
  | Typeof, Symbolic (t, _) -> Val (Type t)
  | Typeof, NOpt (ListExpr, _) -> Val (Type Type.ListType)
  | Typeof, NOpt (TupleExpr, _) -> Val (Type Type.TupleType)
  | Typeof, NOpt (ArrExpr, _) -> Val (Type Type.ArrayType)
  | Typeof, Curry (_, _) -> Val (Type Type.CurryType)
  | Typeof, op ->
      let t = Value_typing.type_of op in
      Val (Type (Option.value_exn t))
  | Sconcat, NOpt (ListExpr, vs) -> reduce_sconcat vs
  | FloatOfString, UnOpt (FloatToString, x) -> x
  (* Unsound *)
  | FloatToString, UnOpt (ToUint32, v) -> v
  (* | ToUint32, Symbolic (Type.FltType, x) -> Symbolic (Type.FltType, x) *)
  | LSort, NOpt (ListExpr, l) when List.length l <= 1 -> NOpt (ListExpr, l)
  | Trim, UnOpt (FloatToString, v) -> UnOpt (FloatToString, v)
  | op', v1' -> UnOpt (op', v1')

let is_loc = function Val (Loc _) -> true | _ -> false

let reduce_binop (op : bopt) (v1 : value) (v2 : value) : value =
  match (op, v1, v2) with
  | op, Val v1, Val v2 -> Val (Eval_op.eval_binopt_expr op v1 v2)
  (* int_to_float(s_len_u(symbolic (__$Str, "s1"))) < 0.  *)
  | ( Lt,
      UnOpt (IntToFloat, UnOpt (StringLenU, Symbolic (Type.StrType, _))),
      Val (Flt 0.0) ) ->
      Val (Bool false)
  | ( Ge,
      UnOpt (IntToFloat, UnOpt (StringLenU, Symbolic (Type.StrType, _))),
      Val (Flt 4294967296.0) ) ->
      Val (Bool false)
  | Eq, NOpt (_v1_t, list1), NOpt (_v2_t, list2) ->
      reduce_list_compare list1 list2
  | Eq, Symbolic _ , Val (Symbol _) -> Val (Bool false)
  | Eq, v, Val (Flt x)
    when is_symbolic v
         && (Float.is_inf x || Float.(x = neg_infinity) || Float.is_nan x) ->
      Val (Bool false)
  | Eq, NOpt (_, _), Val Null -> Val (Bool false)
  | Eq, v, Val Null when Stdlib.not (is_loc v) -> Val (Bool false)
  | Eq, UnOpt (Sconcat, NOpt (_, l1)), UnOpt (Sconcat, NOpt (ListExpr, l2)) -> (
      match (l1, l2) with
      | [ pre1; target1 ], [ pre2; target2 ] when Value.equal pre1 pre2 ->
          BinOpt (Eq, target1, target2)
      | _, _ -> BinOpt (Eq, v1, v2))
  | Eq, v1, v2 when Stdlib.not (is_symbolic v1 || is_symbolic v2) ->
      Val (Bool (Value.equal v1 v2))
  | ( Eq,
      UnOpt (FloatToString, Symbolic (Type.FltType, n1)),
      UnOpt (FloatToString, Symbolic (Type.FltType, n2)) ) ->
      BinOpt (Eq, Symbolic (Type.FltType, n1), Symbolic (Type.FltType, n2))
  | Eq, Val (Str s), UnOpt (FloatToString, Symbolic (Type.FltType, n))
  | Eq, UnOpt (FloatToString, Symbolic (Type.FltType, n)), Val (Str s) -> (
      let s' = Operators.float_of_string (Str s) in
      match s' with
      | Flt v when Val.equal (Flt v) (Flt Float.nan) -> Val (Bool false)
      | _ -> BinOpt (Eq, Symbolic (Type.FltType, n), Val s'))
  | Tnth, NOpt (TupleExpr, vs), Val (Int i) -> List.nth_exn vs i
  | Lnth, NOpt (ListExpr, vs), Val (Int i) -> List.nth_exn vs i
  | Lconcat, NOpt (ListExpr, vs1), NOpt (ListExpr, vs2) ->
      NOpt (ListExpr, vs1 @ vs2)
  | Lconcat, lst, NOpt (ListExpr, []) -> lst
  | Lconcat, NOpt (ListExpr, []), lst -> lst
  | Lprepend, v1, NOpt (ListExpr, vs) -> NOpt (ListExpr, v1 :: vs)
  | Ladd, NOpt (ListExpr, vs), v2 -> NOpt (ListExpr, vs @ [ v2 ])
  | InList, v1, NOpt (ListExpr, vs) -> Val (Bool (Stdlib.List.mem v1 vs))
  | op', v1', v2' -> BinOpt (op', v1', v2')

let reduce_triop (op : topt) (v1 : value) (v2 : value) (v3 : value) : value =
  match (op, v1, v2, v3) with
  | op, Val v1, Val v2, Val v3 -> Val (Eval_op.eval_triopt_expr op v1 v2 v3)
  | Lset, NOpt (ListExpr, vs), Val (Int v2'), _ -> reduce_list_set vs v2' v3
  | _ -> TriOpt (op, v1, v2, v3)

let reduce_nop (op : nopt) (vs : value list) : value = NOpt (op, vs)

let rec reduce (e : value) : value =
  match e with
  | Val v -> Val v
  | UnOpt (op, e) ->
      let v = reduce e in
      reduce_unop op v
  | BinOpt (op, e1, e2) ->
      let v1 = reduce e1 in
      let v2 = reduce e2 in
      let reduced_op = reduce_binop op v1 v2 in
      if Value.equal reduced_op e then reduced_op else reduce reduced_op
  | TriOpt (op, e1, e2, e3) ->
      let v1 = reduce e1 in
      let v2 = reduce e2 in
      let v3 = reduce e3 in
      reduce_triop op v1 v2 v3
  | NOpt (op, es) ->
      let vs = List.map ~f:reduce es in
      reduce_nop op vs
  | Curry (f, es) -> Curry (f, List.map ~f:reduce es)
  | Symbolic (t, x) -> Symbolic (t, reduce x)

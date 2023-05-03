open Core
open Expr
open Val
open Operators

let reduce_sconcat (vs : Expr.t list) : Expr.t = 
  let s =
    List.fold_left vs ~init:[] ~f:(fun a b ->
        match a with
        | [] -> [ b ]
        | h :: t -> (
            match (h, b) with
            | Val (Str h'), Val (Str b') ->
                Val (Str (String.concat ~sep:"" [ h'; b' ])) :: t
            | Val (Str h'), _ -> b :: a
            | _, Val (Str b') ->
                b :: a
            | _ ->
                failwith
                  ("impossible argument types for concat " ^ Expr.str h)
            ))
  in
  let s = List.rev s in
  if List.length s > 1 then UnOpt (Sconcat, NOpt (ListExpr, s))
  else
    Val
      (Str
          (String.concat ~sep:""
            (List.fold_left vs ~init:[] ~f:(fun a b ->
                  match b with Val (Str s) -> a @ [ s ] | _ -> a))))


let reduce_unop (op : uopt) (v : Expr.t) : Expr.t =
  match (op, v) with
  | op, Val v -> Val (Eval_op.eval_unop op v)
  | Neg, Symbolic (_, _) -> UnOpt (Neg, v)
  | IsNaN, Symbolic _ -> Val (Bool false)
  | Not, v' -> UnOpt (Not, v)
  | Head, NOpt (ListExpr, a :: _) -> a
  | Tail, NOpt (ListExpr, _ :: tl) -> NOpt (ListExpr, tl)
  | First, NOpt (TupleExpr, a :: _) -> a
  | Second, NOpt (TupleExpr, _ :: b :: _) -> b
  | ListLen, NOpt (ListExpr, vs) -> Val (Int (List.length vs))
  | TupleLen, NOpt (TupleExpr, vs) -> Val (Int (List.length vs))
  | LSort, NOpt (ListExpr, []) -> NOpt (ListExpr, [])
  | Typeof, Symbolic (t, _) -> Val (Type t)
  | Typeof, NOpt (ListExpr, _) -> Val (Type Type.ListType)
  | Typeof, NOpt (TupleExpr, _) -> Val (Type Type.TupleType)
  | Typeof, NOpt (ArrExpr, _) -> Val (Type Type.ArrayType)
  | Typeof, Curry (_, _) -> Val (Type Type.CurryType)
  | Typeof, op ->
      let t = Sval_typing.type_of op in
      Val (Type (Option.value_exn t))
  | Sconcat, NOpt (ListExpr, vs) -> reduce_sconcat vs
  | FloatOfString, UnOpt (FloatToString, Symbolic (t, x)) -> Symbolic (t, x)
  (* missing obj_to_list, obj_fields*)
  | op', v1' -> UnOpt (op', v1')

let reduce_list_compare (list1 : Expr.t list) (list2 : Expr.t list) : Expr.t = 
  if (List.length list1 = List.length list2) then
    let comp val1 val2 =
      match (val1, val2) with
      | Symbolic(tp1, n1), Symbolic(tp2, n2) -> BinOpt(Eq, n1, n2)
      | Symbolic(_, n), Val _ -> BinOpt(Eq, n, val2)
      | Val _ , Symbolic(_, n) -> BinOpt(Eq, val1, n)
      | Val val1, Val val2 ->  Val (Bool (Val.equal val1 val2))
      | _ -> failwith "incompatible type for eq"
    in
    let concat curr v = 
      match curr, v with 
      | _, BinOpt (_,_,_) -> BinOpt(Log_And, curr, v)
      | BinOpt (_,_,_), _ -> BinOpt(Log_And, curr, v)
      | Val (Bool b1), Val (Bool b2)  -> Val (Operators.log_and ((Bool b1), (Bool b2)))
      | _ -> failwith "wrong"
    in
    let rec fold_until l1 l2 acc = 
      match l1, l2 with
      | [], [] -> acc
      | h1::t1, h2::t2 -> (
        let res = comp h1 h2 in
        match res with
        | Val (Bool false) -> res
        | Val (Bool true) | BinOpt (_,_,_) -> 
            fold_until t1 t2 (concat acc res)
        | _ -> failwith "impossible comparison result")
      | _ -> failwith "lists must have same length"
      
    in fold_until list1 list2 (Val (Bool true))
  else
    Val (Bool false)
  
let reduce_list_set (list : Expr.t list) (idx : int) (newVal : Expr.t) : Expr.t =
  let rec list_set_aux ((v1, v2, v3, v4) : Expr.t list * Val.t * Expr.t * Val.t) : Expr.t list =
  match (v1, v2, v3, v4) with
  | head::tail, Int idx, x, Int n ->
      if n = idx then x :: tail
      else head :: list_set_aux (tail, Int idx, x, Int (n + 1))
  | _ ->
      invalid_arg
        "Exception in Oper.list_set: this operation is only applicable to List \
        and Int arguments"
  in

  if idx >= 0 && idx < List.length list then
    Expr.NOpt(ListExpr, (list_set_aux (list, Int idx, newVal, Int 0)))
  else
    invalid_arg
          "Exception in Oper.list_set: this operation is only applicable to \
          List, Int greater or equal to 0 and Any arguments"

let reduce_binop (op : bopt) (v1 : Expr.t) (v2 : Expr.t) : Expr.t =
  match (op, v1, v2) with
  | op, Val v1, Val v2 -> Val (Eval_op.eval_binopt_expr op v1 v2)
  | Eq, NOpt(v1_t, list1), NOpt(v2_t, list2) -> reduce_list_compare list1 list2
  | Eq, v, Val (Symbol _) when is_symbolic v -> Val (Bool false)
  | Eq, v, Val (Flt x)
    when is_symbolic v
         && (Float.is_inf x || Float.(x = neg_infinity) || Float.is_nan x) ->
      Val (Bool false)
  | Eq, NOpt (_, _), Val Null -> Val (Bool false)
  | Eq, v, Val Null when Caml.not (Expr.is_loc v) -> Val (Bool false)
  | Eq, v1, v2 when Caml.not (is_symbolic v1 || is_symbolic v2) ->
      Val (Bool (Expr.equal v1 v2))
  | Eq, Symbolic(_, n1), Symbolic(_, n2) -> Val (Bool (Expr.equal v1 v2))
  | Tnth, NOpt (TupleExpr, vs), Val (Int i) -> List.nth_exn vs i
  | Lnth, NOpt (ListExpr, vs), Val (Int i) -> List.nth_exn vs i
  | Lconcat, NOpt (ListExpr, vs1), NOpt (ListExpr, vs2) ->
      NOpt (ListExpr, vs1 @ vs2)
  | Lprepend, v1, NOpt (ListExpr, vs) -> NOpt (ListExpr, v1 :: vs)
  | Ladd, NOpt (ListExpr, vs), v2 -> NOpt (ListExpr, vs @ [ v2 ])
  | InList, v1, NOpt (ListExpr, vs) -> Val (Bool (Caml.List.mem v1 vs))
  | op', v1', v2' -> BinOpt (op', v1', v2')

let reduce_triop (op : topt) (v1 : Expr.t) (v2 : Expr.t) (v3 : Expr.t) : Expr.t
    =
  match (op, v1, v2, v3) with
  | op, Val v1, Val v2, Val v3 -> Val (Eval_op.eval_triopt_expr op v1 v2 v3)
  | Lset, NOpt (ListExpr, vs), Val (Int v2'), _ -> reduce_list_set vs v2' v3
  | _ -> TriOpt (op, v1, v2, v3)

let reduce_nop (op : nopt) (vs : Expr.t list) : Expr.t = NOpt (op, vs)

let rec reduce_expr ?(at = Source.no_region) (store : Sstore.t) (e : Expr.t) :
    Expr.t =
  match e with
  | Val v -> Val v
  | Var x -> (
      match Sstore.find store x with
      | Some v -> v
      | None -> failwith ("Cannot find var '" ^ x ^ "'"))
  | UnOpt (op, e) ->
      let v = reduce_expr ~at store e in
      reduce_unop op v
  | BinOpt (op, e1, e2) ->
      let v1 = reduce_expr ~at store e1 and v2 = reduce_expr ~at store e2 in
      reduce_binop op v1 v2
  | TriOpt (op, e1, e2, e3) ->
      let v1 = reduce_expr ~at store e1
      and v2 = reduce_expr ~at store e2
      and v3 = reduce_expr ~at store e3 in
      reduce_triop op v1 v2 v3
  | NOpt (op, es) ->
      let vs = List.map ~f:(reduce_expr ~at store) es in
      reduce_nop op vs
  | Curry (f, es) -> Curry (f, List.map ~f:(reduce_expr ~at store) es)
  | Symbolic (t, x) -> Symbolic (t, reduce_expr ~at store x)

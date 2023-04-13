open Core
open Expr
open Val
open Operators

let reduce_unop (op : uopt) (v : Expr.t) : Expr.t =
  match v with
  | Val v' -> Val (Eval_op.eval_unop op v')
  | _ -> 
    match (op, v) with
    | Neg, Symbolic (_, _) -> UnOpt (Neg, v)
    | Head, NOpt (ListExpr, a :: _) -> a
    | Tail, NOpt (ListExpr, _ :: tl) -> NOpt (ListExpr, tl)
    | First, NOpt (TupleExpr, a :: _) -> a
    | Second, NOpt (TupleExpr, _ :: b :: _) -> b
    | ListLen, NOpt (ListExpr, vs) -> Val (Int (List.length vs))
    | TupleLen, NOpt (TupleExpr, vs) -> Val (Int (List.length vs))
    | Typeof, Symbolic (t, _) -> Val (Type t)
    | Typeof, NOpt (ListExpr, _) -> Val (Type Type.ListType)
    | Typeof, NOpt (TupleExpr, _) -> Val (Type Type.TupleType)
    | Typeof, NOpt (ArrExpr, _) -> Val (Type Type.ArrayType)
    | Typeof, Curry (_, _) -> Val (Type Type.CurryType)
    | Typeof, op ->
        let t = Sval_typing.type_of op in
        Val (Type (Option.value_exn t))
    | Sconcat, NOpt (ListExpr, vs) ->
        Val
          (Str
            (String.concat ~sep:""
                (List.fold_left vs ~init:[] ~f:(fun a b ->
                    match b with Val (Str s) -> a @ [ s ] | _ -> a))))
    | FloatOfString, UnOpt (FloatToString, Symbolic (t, x)) -> Symbolic (t, x)
    | Trim, Symbolic (Type.StrType, _) -> UnOpt (op, v)
    | op', v1' -> UnOpt (op', v1')

let reduce_binop (op : bopt) (v1 : Expr.t) (v2 : Expr.t) : Expr.t =
  match (v1, v2) with
  | Val v1', Val v2' -> Val (Eval_op.eval_binopt_expr op v1' v2')
  | _ -> 
    match (op, v1, v2) with
    | Eq, v', Val Null when is_symbolic v' -> Val (Bool false)
    | Eq, Val v', Val (Symbol _) when Caml.not (is_symbol v') -> Val (Bool false)
    | Eq, v, Val (Symbol _) when is_symbolic v -> Val (Bool false)
    | Eq, _, _ when Caml.not (is_symbolic v1 || is_symbolic v2) ->
        Val (Bool (Expr.equal v1 v2))
    | Tnth, NOpt (TupleExpr, vs), Val (Int i) -> List.nth_exn vs i
    | Lnth, NOpt (ListExpr, vs), Val (Int i) -> List.nth_exn vs i
    | InList, v1, NOpt (ListExpr, vs) -> Val (Bool (Caml.List.mem v1 vs))
    | Lprepend, v1, NOpt (ListExpr, vs) -> NOpt (ListExpr, v1 :: vs)
    | Ladd, NOpt (ListExpr, vs), v2 -> NOpt (ListExpr, vs @ [ v2 ])
    (*missing InObj | InList*)
    | op', v1', v2' -> BinOpt (op', v1', v2')

let reduce_triop (op : topt) (v1 : Expr.t) (v2 : Expr.t) (v3 : Expr.t) : Expr.t
    =
  match (v1, v2, v3) with
  | Val v1', Val v2', Val v3' -> Val (Eval_op.eval_triopt_expr op v1' v2' v3')
  | _ -> TriOpt (op, v1, v2, v3)

let reduce_nop (op : nopt) (vs : Expr.t list) : Expr.t = NOpt (op, vs)

open Core
open Expr
open Val
open Operators

let reduce_unop (op : uopt) (v : Expr.t) : Expr.t =
  match (op, v) with
  | Neg, Val v' -> (
      match v' with
      | Flt f -> Val (Flt (-.f))
      | Int i -> Val (Int (-i))
      | _ -> UnOpt (Neg, v))
  | Not, Val (Bool b) -> Val (Bool (Caml.Bool.not b))
  | Not, v' -> UnOpt (Not, v)
  | Head, NOpt (ListExpr, a :: _) -> a
  | Tail, NOpt (ListExpr, _ :: tl) -> NOpt (ListExpr, tl)
  | First, NOpt (TupleExpr, a :: _) -> a
  | Second, NOpt (TupleExpr, _ :: b :: _) -> b
  | ListLen, NOpt (ListExpr, vs) -> Val (Int (List.length vs))
  | TupleLen, NOpt (TupleExpr, vs) -> Val (Int (List.length vs))
  | IntToFloat, Val (Int i) -> Val (Flt (Float.of_int i))
  | IntToString, Val (Int i) -> Val (Str (Int.to_string i))
  | FloatToString, Val (Flt f) -> Val (Str (Float.to_string f))
  | FloatToString, Symbolic (_, _) -> UnOpt (FloatToString, v)
  | Typeof, v -> failwith "TODO"
  | Sconcat, NOpt (ListExpr, vs) ->
      Val
        (Str
           (String.concat ~sep:""
              (List.fold_left vs ~init:[] ~f:(fun a b ->
                   match b with Val (Str s) -> a @ [ s ] | _ -> a))))
  | Trim, Val (Str s) -> Val (Str (String_utils.trim s))
  | StringLen, Val (Str s) -> Val (Int (String.length s))
  | StringLen, Symbolic (Type.StrType, _) -> UnOpt (StringLen, v)
  | StringLenU, Val (Str s) -> Val (Int (String_utils.s_len_u s))
  | FloatOfString, Val (Str s) ->
      let trimmed = String.strip s in
      Val
        (if String.length trimmed = 0 then Flt Float.nan
        else try Flt (Float.of_string trimmed) with exn -> Flt Float.nan)
  | FloatOfString, UnOpt (FloatToString, Symbolic (t, x)) -> Symbolic (t, x)
  | Exp, Val (Flt f) -> Val (Flt (Float.exp f))
  | Log_e, Val (Flt f) -> Val (Flt (Float.log f))
  | Log_10, Val (Flt f) -> Val (Flt (Float.log10 f))
  | Sqrt, Val (Flt f) -> Val (Flt (Float.sqrt f))
  | Abs, Val (Flt f) -> Val (Flt (Float.abs f))
  | Floor, Val (Flt f) -> Val (Flt (Float.round_down f))
  | ToInt, Val (Flt f) -> Val (Flt (Arith_utils.to_int f))
  | ToUint32, Val (Flt f) -> Val (Flt (Arith_utils.to_uint32 f))
  | IsNaN, Val (Flt n) -> Val (Bool (Float.is_nan n))
  | IsNaN, v' -> BinOpt (Eq, v', Val (Flt Float.nan))
  | _ ->
      (* TODO: clear error message *)
      invalid_arg
        ("ill-typed or not implemented UnOpt: '" ^ str_of_unopt op ^ "'")

let reduce_binop (op : bopt) (v1 : Expr.t) (v2 : Expr.t) : Expr.t =
  match (op, v1, v2) with
  | Plus, Val (Int i1), Val (Int i2) -> Val (Int (i1 + i2))
  | Plus, Val (Flt f1), Val (Flt f2) -> Val (Flt (f1 +. f2))
  | Minus, Val (Int i1), Val (Int i2) -> Val (Int (i1 - i2))
  | Minus, Val (Flt f1), Val (Flt f2) -> Val (Flt (f1 -. f2))
  | Times, Val (Int i1), Val (Int i2) -> Val (Int (i1 * i2))
  | Times, Val (Flt f1), Val (Flt f2) -> Val (Flt (f1 *. f2))
  | Div, Val (Int i1), Val (Int i2) -> Val (Int (i1 / i2))
  | Div, Val (Flt f1), Val (Flt f2) -> Val (Flt (f1 /. f2))
  | Modulo, Val (Flt f1), Val (Flt f2) -> Val (Flt (Float.mod_float f1 f2))
  | Eq, Val (Flt f1), Val (Flt f2) -> Val (Bool (Float.equal f1 f2))
  | Eq, Val (Arr a1), Val (Arr a2) -> Val (Bool (Caml.( == ) a1 a2))
  | Eq, v', Val Null when is_symbolic v' -> Val (Bool false)
  | Eq, Val v', Val (Symbol _) when Caml.not (is_symbol v') -> Val (Bool false)
  | Eq, _, _ when Caml.not (is_symbolic v1 || is_symbolic v2) ->
      Val (Bool (Caml.( = ) v1 v2))
  | Lt, Val (Int i1), Val (Int i2) -> Val (Bool (i1 < i2))
  | Lt, Val (Flt f1), Val (Flt f2) -> Val (Bool Float.(f1 < f2))
  | Gt, Val (Int i1), Val (Int i2) -> Val (Bool (i1 > i2))
  | Gt, Val (Flt f1), Val (Flt f2) -> Val (Bool Float.(f1 > f2))
  | Le, Val (Int i1), Val (Int i2) -> Val (Bool (i1 <= i2))
  | Le, Val (Flt f1), Val (Flt f2) -> Val (Bool Float.(f1 <= f2))
  | Ge, Val (Int i1), Val (Int i2) -> Val (Bool (i1 >= i2))
  | Ge, Val (Flt f1), Val (Flt f2) -> Val (Bool Float.(f1 >= f2))
  | Min, Val (Flt f1), Val (Flt f2) -> Val (Flt (Float.min f1 f2))
  | Max, Val (Flt f1), Val (Flt f2) -> Val (Flt (Float.max f1 f2))
  | Pow, Val (Flt f1), Val (Flt f2) -> Val (Flt Float.(f1 ** f2))
  | Log_And, Val (Bool b1), Val (Bool b2) -> Val (Bool (b1 && b2))
  | Log_Or, Val (Bool b1), Val (Bool b2) -> Val (Bool (b1 || b2))
  | Tnth, NOpt (TupleExpr, vs), Val (Int i) -> List.nth_exn vs i
  | Lnth, NOpt (ListExpr, vs), Val (Int i) -> List.nth_exn vs i
  | InList, v1, NOpt (ListExpr, vs) -> Val (Bool (Caml.List.mem v1 vs))
  | Lprepend, v1, NOpt (ListExpr, vs) -> NOpt (ListExpr, v1 :: vs)
  | Ladd, NOpt (ListExpr, vs), v2 -> NOpt (ListExpr, vs @ [ v2 ])
  | Snth, Val (Str s), Val (Int i) ->
      Val (Str (String.nget s i |> String.of_char))
  | op', v1', v2' -> BinOpt (op', v1', v2')

let reduce_triop (op : topt) (v1 : Expr.t) (v2 : Expr.t) (v3 : Expr.t) : Expr.t
    =
  TriOpt (op, v1, v2, v3)

let reduce_nop (op : nopt) (vs : Expr.t list) : Expr.t = NOpt (op, vs)

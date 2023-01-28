open Sval
open Type
open Encoding
open Operators

let solver = mk_solver ()
let encode e = try ignore (encode_value e) with exn -> raise exn
let int = Int 42
let flt = Flt 42.
let str = Str "42"
let byte = Byte 42
let bool = Bool true
let symb = Symbol "empty"
let sint = Symbolic (IntType, "x")
let sflt = Symbolic (FltType, "x")
let sbool = Symbolic (BoolType, "x")

let%test_unit _ = encode int
let%test_unit _ = encode flt
let%test_unit _ = encode str
let%test_unit _ = encode byte
let%test_unit _ = encode bool
let%test_unit _ = encode symb
let%test_unit _ = encode sint
let%test_unit _ = encode sflt
let%test_unit _ = encode sbool
(* Unary ops *)
let%test_unit _ = encode (Unop (Neg, flt))
let%test_unit _ = encode (Unop (Not, bool))
let%test_unit _ = encode (Unop (IsNaN, flt))
let%test_unit _ = encode (Unop (StringLen, str))
let%test_unit _ = encode (Unop (IntToFloat, int))
let%test_unit _ = encode (Unop (IntToString, int))
let%test_unit _ = encode (Unop (IntToFourHex, int))
let%test_unit _ = encode (Unop (IntOfString, str))
let%test_unit _ = encode (Unop (IntOfFloat, flt))
let%test_unit _ = encode (Unop (FloatOfString, str))
let%test_unit _ = encode (Unop (FloatToString, flt))
let%test_unit _ = encode (Unop (Abs, int))
let%test_unit _ = encode (Unop (Acos, flt))
let%test_unit _ = encode (Unop (Asin, flt))
let%test_unit _ = encode (Unop (Atan, flt))
let%test_unit _ = encode (Unop (Ceil, flt))
let%test_unit _ = encode (Unop (Cos, flt))
let%test_unit _ = encode (Unop (Exp, flt))
let%test_unit _ = encode (Unop (Floor, flt))
let%test_unit _ = encode (Unop (Log_e, flt))
let%test_unit _ = encode (Unop (Log_10, flt))
let%test_unit _ = encode (Unop (Random, flt))
let%test_unit _ = encode (Unop (Sin, flt))
let%test_unit _ = encode (Unop (Sqrt, flt))
let%test_unit _ = encode (Unop (Tan, flt))
let%test_unit _ = encode (Unop (Cosh, flt))
let%test_unit _ = encode (Unop (Sinh, flt))
let%test_unit _ = encode (Unop (Tanh, flt))
(* Binary ops *)
let%test_unit _ = encode (Binop (Plus, int, int))
let%test_unit _ = encode (Binop (Minus, flt, flt))
let%test_unit _ = encode (Binop (Times, sint, int))
let%test_unit _ = encode (Binop (Div, sflt, flt))
let%test_unit _ = encode (Binop (Modulo, sint, int))
let%test_unit _ = encode (Binop (Eq, sbool, bool))
let%test_unit _ = encode (Binop (Lt, sint, int))
let%test_unit _ = encode (Binop (Gt, sflt, flt))
let%test_unit _ = encode (Binop (Le, sflt, flt))
let%test_unit _ = encode (Binop (Ge, sint, int))
let%test_unit _ = encode (Binop (Log_And, bool, bool))
let%test_unit _ = encode (Binop (Log_Or, bool, bool))
let%test_unit _ = encode (Binop (Atan2, flt, sflt))
let%test_unit _ = encode (Binop (Min, sflt, sflt))
let%test_unit _ = encode (Binop (Max, flt, flt))
let%test_unit _ = encode (Binop (Pow, flt, sflt))
(* Satisfiability *)
let%test _ = check solver [ Binop (Gt, sint, int) ]
let%test _ = check solver [ Binop (Gt, sflt, flt) ]

open Test

let test_congruency = TypeCheck.test_congruency

(* ========== Any Type ========== *)

let%test "congruency_any_eq" = test_congruency (t_any, t_any) (Ok ())
let%test "congruency_any_ref" = test_congruency (t_any, t_int) (Ok ())
let%test "congruency_any_src" = test_congruency (t_int, t_any) (Ok ())

(* ========== Unknown Type ========== *)

let%test "congruency_unknown_eq" = test_congruency (t_unknown, t_unknown) (Ok ())

let%test "congruency_unknown_ref" =
  test_congruency (t_unknown, t_int) (Error [ BadCongruency (t_unknown, t_int) ])

let%test "congruency_unknown_src" =
  test_congruency (t_int, t_unknown) (Error [ BadCongruency (t_int, t_unknown) ])

(* ========== Never Type ========== *)

let%test "congruency_never_eq" = test_congruency (t_never, t_never) (Ok ())

let%test "congruency_never_ref" =
  test_congruency (t_never, t_int) (Error [ BadCongruency (t_never, t_int) ])

let%test "congruency_never_src" =
  test_congruency (t_int, t_never) (Error [ BadCongruency (t_int, t_never) ])

(* ========== Undefined Type ========== *)

let%test "congruency_undefined_eq" =
  test_congruency (t_undefined, t_undefined) (Ok ())

let%test "congruency_undefined_ref" =
  test_congruency (t_undefined, t_int)
    (Error [ BadCongruency (t_undefined, t_int) ])

let%test "congruency_undefined_src" =
  test_congruency (t_int, t_undefined)
    (Error [ BadCongruency (t_int, t_undefined) ])

(* ========== Null Type ========== *)

let%test "congruency_null_eq" = test_congruency (t_null, t_null) (Ok ())

let%test "congruency_null_ref" =
  test_congruency (t_null, t_int) (Error [ BadCongruency (t_null, t_int) ])

let%test "congruency_null_src" =
  test_congruency (t_int, t_null) (Error [ BadCongruency (t_int, t_null) ])

(* ========== Void Type ========== *)

let%test "congruency_void_eq" = test_congruency (t_void, t_void) (Ok ())

let%test "congruency_void_ref" =
  test_congruency (t_void, t_int) (Error [ BadCongruency (t_void, t_int) ])

let%test "congruency_void_src" =
  test_congruency (t_int, t_void) (Error [ BadCongruency (t_int, t_void) ])

(* ========== Int Type ========== *)

let%test "congruency_int_eq" = test_congruency (t_int, t_int) (Ok ())

let%test "congruency_int_ref" =
  test_congruency (t_int, t_null) (Error [ BadCongruency (t_int, t_null) ])

let%test "congruency_int_src" =
  test_congruency (t_null, t_int) (Error [ BadCongruency (t_null, t_int) ])

let%test "congruency_int_literal" =
  test_congruency
    (t_int, lt_integer 10)
    (Error [ BadCongruency (t_int, lt_integer 10) ])

(* ========== Float Type ========== *)

let%test "congruency_float_eq" = test_congruency (t_float, t_float) (Ok ())

let%test "congruency_float_ref" =
  test_congruency (t_float, t_null) (Error [ BadCongruency (t_float, t_null) ])

let%test "congruency_float_src" =
  test_congruency (t_null, t_float) (Error [ BadCongruency (t_null, t_float) ])

let%test "congruency_float_literal" =
  test_congruency
    (t_float, lt_float 10.1)
    (Error [ BadCongruency (t_float, lt_float 10.1) ])

(* ========== String Type ========== *)

let%test "congruency_string_eq" = test_congruency (t_string, t_string) (Ok ())

let%test "congruency_string_ref" =
  test_congruency (t_string, t_null) (Error [ BadCongruency (t_string, t_null) ])

let%test "congruency_string_src" =
  test_congruency (t_null, t_string) (Error [ BadCongruency (t_null, t_string) ])

let%test "congruency_string_literal" =
  test_congruency
    (t_string, lt_string "abc")
    (Error [ BadCongruency (t_string, lt_string "abc") ])

(* ========== Boolean Type ========== *)

let%test "congruency_boolean_eq" = test_congruency (t_boolean, t_boolean) (Ok ())

let%test "congruency_boolean_ref" =
  test_congruency (t_boolean, t_null)
    (Error [ BadCongruency (t_boolean, t_null) ])

let%test "congruency_boolean_src" =
  test_congruency (t_null, t_boolean)
    (Error [ BadCongruency (t_null, t_boolean) ])

let%test "congruency_boolean_literal" =
  test_congruency
    (t_boolean, lt_boolean true)
    (Error [ BadCongruency (t_boolean, lt_boolean true) ])

(* ========== Symbol Type ========== *)

let%test "congruency_symbol_eq" = test_congruency (t_symbol, t_symbol) (Ok ())

let%test "congruency_symbol_ref" =
  test_congruency (t_symbol, t_null) (Error [ BadCongruency (t_symbol, t_null) ])

let%test "congruency_symbol_src" =
  test_congruency (t_null, t_symbol) (Error [ BadCongruency (t_null, t_symbol) ])

let%test "congruency_symbol_literal" =
  test_congruency
    (t_symbol, lt_symbol "a")
    (Error [ BadCongruency (t_symbol, lt_symbol "a") ])

(* ========== Literal Type ========== *)

let%test "congruency_literal_eq" =
  test_congruency (lt_integer 10, lt_integer 10) (Ok ())

let%test "congruency_literal_badval" =
  test_congruency
    (lt_integer 10, lt_integer 20)
    (Error [ BadCongruency (lt_integer 10, lt_integer 20) ])

let%test "congruency_literal_badtype" =
  test_congruency
    (lt_integer 10, lt_string "abc")
    (Error [ BadCongruency (lt_integer 10, lt_string "abc") ])

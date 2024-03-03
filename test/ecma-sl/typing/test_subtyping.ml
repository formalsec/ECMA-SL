open Test

let test_subtyping = TypeCheck.test_subtyping

(* ========== Any Type ========== *)

let%test "subtyping_any_eq" = test_subtyping (t_any, t_any) (Ok ())
let%test "subtyping_any_ref" = test_subtyping (t_any, t_int) (Ok ())
let%test "subtyping_any_src" = test_subtyping (t_int, t_any) (Ok ())

(* ========== Unknown Type ========== *)

let%test "subtyping_unknown_eq" = test_subtyping (t_unknown, t_unknown) (Ok ())
let%test "subtyping_unknown_ref" = test_subtyping (t_unknown, t_int) (Ok ())

let%test "subtyping_unknown_src" =
  test_subtyping (t_int, t_unknown) (Error [ BadSubtyping (t_int, t_unknown) ])

(* ========== Never Type ========== *)

let%test "subtyping_never_eq" = test_subtyping (t_never, t_never) (Ok ())

let%test "subtyping_never_ref" =
  test_subtyping (t_never, t_int) (Error [ BadSubtyping (t_never, t_int) ])

let%test "subtyping_never_src" = test_subtyping (t_int, t_never) (Ok ())

(* ========== Undefined Type ========== *)

let%test "subtyping_undefined_eq" =
  test_subtyping (t_undefined, t_undefined) (Ok ())

let%test "subtyping_undefined_ref" =
  test_subtyping (t_undefined, t_int)
    (Error [ BadSubtyping (t_undefined, t_int) ])

let%test "subtyping_undefined_src" =
  test_subtyping (t_int, t_undefined)
    (Error [ BadSubtyping (t_int, t_undefined) ])

(* ========== Null Type ========== *)

let%test "subtyping_null_eq" = test_subtyping (t_null, t_null) (Ok ())

let%test "subtyping_null_ref" =
  test_subtyping (t_null, t_int) (Error [ BadSubtyping (t_null, t_int) ])

let%test "subtyping_null_src" =
  test_subtyping (t_int, t_null) (Error [ BadSubtyping (t_int, t_null) ])

(* ========== Void Type ========== *)

let%test "subtyping_void_eq" = test_subtyping (t_void, t_void) (Ok ())

let%test "subtyping_void_ref" =
  test_subtyping (t_void, t_int) (Error [ BadSubtyping (t_void, t_int) ])

let%test "subtyping_void_src" =
  test_subtyping (t_int, t_void) (Error [ BadSubtyping (t_int, t_void) ])

(* ========== Int Type ========== *)

let%test "subtyping_int_eq" = test_subtyping (t_int, t_int) (Ok ())

let%test "subtyping_int_ref" =
  test_subtyping (t_int, t_null) (Error [ BadSubtyping (t_int, t_null) ])

let%test "subtyping_int_src" =
  test_subtyping (t_null, t_int) (Error [ BadSubtyping (t_null, t_int) ])

let%test "subtyping_int_literal" = test_subtyping (t_int, lt_integer 10) (Ok ())

(* ========== Float Type ========== *)

let%test "subtyping_float_eq" = test_subtyping (t_float, t_float) (Ok ())

let%test "subtyping_float_ref" =
  test_subtyping (t_float, t_null) (Error [ BadSubtyping (t_float, t_null) ])

let%test "subtyping_float_src" =
  test_subtyping (t_null, t_float) (Error [ BadSubtyping (t_null, t_float) ])

let%test "subtyping_float_literal" =
  test_subtyping (t_float, lt_float 10.1) (Ok ())

(* ========== String Type ========== *)

let%test "subtyping_string_eq" = test_subtyping (t_string, t_string) (Ok ())

let%test "subtyping_string_ref" =
  test_subtyping (t_string, t_null) (Error [ BadSubtyping (t_string, t_null) ])

let%test "subtyping_string_src" =
  test_subtyping (t_null, t_string) (Error [ BadSubtyping (t_null, t_string) ])

let%test "subtyping_string_literal" =
  test_subtyping (t_string, lt_string "abc") (Ok ())

(* ========== Boolean Type ========== *)

let%test "subtyping_boolean_eq" = test_subtyping (t_boolean, t_boolean) (Ok ())

let%test "subtyping_boolean_ref" =
  test_subtyping (t_boolean, t_null) (Error [ BadSubtyping (t_boolean, t_null) ])

let%test "subtyping_boolean_src" =
  test_subtyping (t_null, t_boolean) (Error [ BadSubtyping (t_null, t_boolean) ])

let%test "subtyping_boolean_literal" =
  test_subtyping (t_boolean, lt_boolean true) (Ok ())

(* ========== Symbol Type ========== *)

let%test "subtyping_symbol_eq" = test_subtyping (t_symbol, t_symbol) (Ok ())

let%test "subtyping_symbol_ref" =
  test_subtyping (t_symbol, t_null) (Error [ BadSubtyping (t_symbol, t_null) ])

let%test "subtyping_symbol_src" =
  test_subtyping (t_null, t_symbol) (Error [ BadSubtyping (t_null, t_symbol) ])

let%test "subtyping_symbol_literal" =
  test_subtyping (t_symbol, lt_symbol "a") (Ok ())

(* ========== Literal Type ========== *)

let%test "subtyping_literal_eq" =
  test_subtyping (lt_integer 10, lt_integer 10) (Ok ())

let%test "subtyping_literal_badval" =
  test_subtyping
    (lt_integer 10, lt_integer 20)
    (Error [ BadSubtyping (lt_integer 10, lt_integer 20) ])

let%test "subtyping_literal_badtype" =
  test_subtyping
    (lt_integer 10, lt_string "abc")
    (Error [ BadSubtyping (lt_integer 10, lt_string "abc") ])

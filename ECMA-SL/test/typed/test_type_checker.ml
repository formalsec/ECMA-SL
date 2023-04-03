(* ======================================== Basic ======================================== *)
let%test _ = Test.type_checker_succ "example/basic/ok_typing.esl"
let%test _ = Test.type_checker_succ "example/basic/ok_typing_propagation.esl"
let%test _ = Test.type_checker_succ "example/basic/ok_primitive.esl"

let%test _ =
  Test.type_checker_fail "example/basic/nok_unknown_var.esl"
    [ T_Err.UnknownVar "bar" ]

let%test _ =
  Test.type_checker_fail "example/basic/nok_primitive_assignment.esl"
    [ T_Err.BadAssignment (E_Type.NumberType, E_Type.StringType) ]

(* ======================================== Functions ======================================== *)
let%test _ = Test.type_checker_succ "example/function/ok_function.esl"
let%test _ = Test.type_checker_succ "example/function/ok_call.esl"
let%test _ = Test.type_checker_succ "example/function/ok_use_param.esl"

let%test _ =
  Test.type_checker_fail "example/function/nok_unknown_func.esl"
    [ T_Err.UnknownFunction "foo" ]

let%test _ =
  Test.type_checker_fail "example/function/nok_duplicate_params.esl"
    [ T_Err.DuplicatedParam "x" ]

let%test _ =
  Test.type_checker_fail "example/function/nok_return.esl"
    [ T_Err.BadReturn (E_Type.StringType, E_Type.NumberType) ]

let%test _ =
  Test.type_checker_fail "example/function/nok_missing_args.esl"
    [ T_Err.MissingArgs (2, 1) ]

let%test _ =
  Test.type_checker_fail "example/function/nok_bad_call.esl"
    [ T_Err.BadArgument (E_Type.NumberType, E_Type.StringType) ]

(* ======================================== Operators ======================================== *)
let%test _ = Test.type_checker_succ "example/op/ok_unop.esl"
let%test _ = Test.type_checker_succ "example/op/ok_binop.esl"
let%test _ = Test.type_checker_succ "example/op/ok_triop.esl"

let%test _ =
  Test.type_checker_fail "example/op/nok_neg.esl"
    [
      T_Err.BadOp (Operators.str_of_unopt Operators.Neg, [ E_Type.StringType ]);
    ]

let%test _ =
  Test.type_checker_fail "example/op/nok_plus_num_str.esl"
    [
      T_Err.BadOp
        ( Operators.str_of_binopt_single Operators.Plus,
          [ E_Type.NumberType; E_Type.StringType ] );
    ]

let%test _ =
  Test.type_checker_fail "example/op/nok_ssubstr.esl"
    [
      T_Err.BadOp
        ( Operators.str_of_triopt_single Operators.Ssubstr,
          [ E_Type.StringType; E_Type.StringType; E_Type.NumberType ] );
    ]

(* ======================================== Nary ======================================== *)
let%test _ = Test.type_checker_succ "example/nary/ok_union.esl"

let%test _ =
  Test.type_checker_fail "example/nary/nok_union.esl"
    [
      T_Err.BadAssignment
        ( E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ],
          E_Type.BooleanType );
    ]

let%test _ =
  Test.type_checker_fail "example/nary/nok_union_simplify.esl"
    [
      T_Err.BadAssignment
        ( E_Type.StringType,
          E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ] );
    ]

(* ======================================== Stmts ======================================== *)
let%test _ = Test.type_checker_succ "example/stmt/ok_ifelse.esl"
let%test _ = Test.type_checker_succ "example/stmt/ok_ifelse_union.esl"
let%test _ = Test.type_checker_succ "example/stmt/ok_while_union.esl"

let%test _ =
  Test.type_checker_fail "example/stmt/nok_ifelse.esl"
    [
      T_Err.BadExpectedType (E_Type.BooleanType, E_Type.NumberType);
      T_Err.BadAssignment (E_Type.StringType, E_Type.NumberType);
      T_Err.BadAssignment (E_Type.BooleanType, E_Type.NumberType);
    ]

let%test _ =
  Test.type_checker_fail "example/stmt/nok_if_unknown.esl"
    [
      T_Err.BadAssignment
        ( E_Type.NumberType,
          E_Type.UnionType [ E_Type.NumberType; E_Type.UnknownType ] );
    ]

let%test _ =
  Test.type_checker_fail "example/stmt/nok_ifelse_union.esl"
    [
      T_Err.BadAssignment
        ( E_Type.NumberType,
          E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ] );
    ]

let%test _ =
  Test.type_checker_fail "example/stmt/nok_while_unknown.esl"
    [
      T_Err.BadAssignment
        ( E_Type.NumberType,
          E_Type.UnionType [ E_Type.NumberType; E_Type.UnknownType ] );
    ]

(* ======================================== Inference ======================================== *)
let%test _ = Test.type_checker_succ "example/inference/ok_assignment.esl"
let%test _ = Test.type_checker_succ "example/inference/ok_union.esl"

let%test _ =
  Test.type_checker_fail "example/inference/nok_assignment.esl"
    [ T_Err.BadAssignment (E_Type.NumberType, E_Type.StringType) ]

(* ======================================== Extra ======================================== *)

let%test _ =
  Test.type_checker_fail "example/extra/nok_ifelse_return.esl"
    [ T_Err.BadReturn (E_Type.NumberType, E_Type.StringType) ]

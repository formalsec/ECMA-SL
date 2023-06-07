open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "example/function/typedFunction.esl"
    [ DuplicatedParam "x" ]

let%test _ =
  Test.type_checker_test "example/function/call.esl"
    [
      NExpectedArgs (2, 1);
      NExpectedArgs (2, 3);
      BadArgument (IntType, StringType);
      UnknownFunction "bar";
    ]

let%test _ =
  Test.type_checker_test "example/function/return.esl"
    [ BadReturn (StringType, IntType) ]

let%test _ =
  Test.type_checker_test "example/function/operator.esl"
    [
      BadOperand (BooleanType, StringType);
      BadOperand (IntType, FloatType);
      BadOperand (IntType, StringType);
      BadOperand (BooleanType, StringType);
      BadOperand (IntType, StringType);
    ]

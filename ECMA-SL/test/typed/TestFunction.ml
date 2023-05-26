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
      BadArgument (NumberType, StringType);
      UnknownFunction "bar";
    ]

let%test _ =
  Test.type_checker_test "example/function/return.esl"
    [ BadReturn (StringType, NumberType) ]

let%test _ =
  Test.type_checker_test "example/function/operator.esl"
    [
      BadOperand (BooleanType, StringType);
      BadOperand (NumberType, StringType);
      BadOperand (BooleanType, StringType);
      BadOperand (NumberType, StringType);
      BadValue (StringType, NumberType);
    ]

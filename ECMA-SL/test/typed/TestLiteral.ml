open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "examples/literal/assign.esl"
    [
      BadValue (LiteralType (Val.Int 10), LiteralType (Val.Int 20));
      BadValue (LiteralType (Val.Int 10), LiteralType (Val.Flt 10.2));
      BadValue (LiteralType (Val.Str "abc"), LiteralType (Val.Int 10));
      BadValue (LiteralType (Val.Int 10), NullType);
    ]

let%test _ =
  Test.type_checker_test "examples/literal/call.esl"
    [
      BadArgument (LiteralType (Val.Int 20), LiteralType (Val.Int 10));
      BadArgument (StringType, IntType);
    ]

let%test _ =
  Test.type_checker_test "examples/literal/narrowing.esl"
    [
      BadValue (LiteralType (Val.Int 20), IntType);
      BadValue (StringType, IntType);
      BadArgument (LiteralType (Val.Int 20), IntType);
      BadArgument (StringType, IntType);
    ]

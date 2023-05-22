open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "example/literal/assign.esl"
    [
      BadValue (LiteralType (Val.Int 10), LiteralType (Val.Int 20));
      BadValue (LiteralType (Val.Int 10), LiteralType (Val.Flt 10.2));
      BadValue (LiteralType (Val.Str "abc"), LiteralType (Val.Int 10));
      BadValue (LiteralType (Val.Int 10), NullType);
    ]

let%test _ =
  Test.type_checker_test "example/literal/call.esl"
    [
      BadArgument (LiteralType (Val.Int 20), LiteralType (Val.Int 10));
      BadArgument (StringType, NumberType);
    ]

let%test _ =
  Test.type_checker_test "example/literal/narrowing.esl"
    [
      BadValue (LiteralType (Val.Int 20), NumberType);
      BadValue (StringType, NumberType);
      BadArgument (LiteralType (Val.Int 20), NumberType);
      BadArgument (StringType, NumberType);
    ]

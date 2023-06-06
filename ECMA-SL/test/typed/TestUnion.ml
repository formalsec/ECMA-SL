open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "example/union/simple.esl"
    [ BadValue (UnionType [ NumberType; StringType ], BooleanType) ]

let%test _ =
  Test.type_checker_test "example/union/merge.esl"
    [
      BadValue (UnionType [ BooleanType; NumberType ], StringType);
      BadValue (UnionType [ NumberType; StringType ], BooleanType);
    ]

let%test _ =
  Test.type_checker_test "example/union/literal.esl"
    [
      BadValue (UnionType [ LiteralType (Val.Int 10); StringType ], NumberType);
      BadValue (UnionType [ LiteralType (Val.Int 10); StringType ], BooleanType);
    ]

let%test _ =
  Test.type_checker_test "example/union/narrowing.esl"
    [
      BadValue (StringType, NumberType);
      BadValue (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (NumberType, UnknownType);
    ]

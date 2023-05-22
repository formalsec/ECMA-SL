open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "example/if_while/ifEval.esl"
    [
      BadExpectedType (BooleanType, NumberType);
      BadValue (StringType, NumberType);
      BadValue (StringType, NumberType);
    ]

let%test _ =
  Test.type_checker_test "example/if_while/ifNarrowing.esl"
    [
      BadValue (NumberType, BooleanType);
      BadValue (NumberType, StringType);
      BadValue (NumberType, UndefinedType);
      BadValue (NumberType, BooleanType);
    ]

let%test _ =
  Test.type_checker_test "example/if_while/whileEval.esl"
    [
      BadExpectedType (BooleanType, NumberType);
      BadTypeUpdate (NumberType, UnionType [ NumberType; StringType ]);
      BadTypeUpdate (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (StringType, NumberType);
      UnknownVar "baz";
    ]

let%test _ =
  Test.type_checker_test "example/if_while/whileNarrowing.esl"
    [
      BadValue (LiteralType (Val.Int 10), NumberType);
      BadValue (LiteralType (Val.Int 20), NumberType);
      BadValue
        ( UnionType [ LiteralType (Val.Int 10); LiteralType (Val.Int 20) ],
          NumberType );
      BadValue
        ( UnionType [ LiteralType (Val.Int 10); LiteralType (Val.Int 20) ],
          NumberType );
    ]

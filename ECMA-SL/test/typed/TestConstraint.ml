open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "example/constraint/typeof.esl"
    [
      BadValue (StringType, NumberType);
      BadValue (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (BooleanType, StringType);
      BadValue (StringType, BooleanType);
      BadPossibleType (Some "baz", NullType);
      BadValue (NullType, Test.obj_cons [ ("f", NumberType) ]);
    ]

let%test _ =
  Test.type_checker_test "example/constraint/eq.esl"
    [
      BadValue (LiteralType (Val.Int 20), LiteralType (Val.Int 10));
      BadValue (LiteralType (Val.Int 10), NumberType);
      BadValue (LiteralType (Val.Int 20), NumberType);
      BadValue (LiteralType (Val.Bool false), LiteralType (Val.Bool true));
      BadValue (LiteralType (Val.Bool true), LiteralType (Val.Bool false));
      BadPossibleType (Some "baz", NullType);
      BadValue (NullType, Test.obj_cons [ ("f", NumberType) ]);
    ]

let%test _ =
  Test.type_checker_test "example/constraint/objectUnion.esl"
    [
      BadValue (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (StringType, NumberType);
      BadValue (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (StringType, NumberType);
      BadValue (NumberType, StringType);
      BadValue (NumberType, StringType);
    ]

let%test _ =
  Test.type_checker_test "example/constraint/overlap.esl"
    [
      BadValue (NumberType, NeverType);
      BadValue (StringType, NeverType);
      NoOverlapComp (LiteralType (Val.Int 10), LiteralType (Val.Int 20));
      NoOverlapComp (LiteralType (Val.Int 20), LiteralType (Val.Int 10));
    ]

    let%test _ =
  Test.type_checker_test "example/constraint/combine.esl"
    [
      BadValue (NumberType, BooleanType);
      BadValue (StringType, NumberType);
      BadValue (BooleanType, NumberType);
      BadValue (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (BooleanType, NumberType);
      BadValue (LiteralType (Val.Str "abc"), LiteralType (Val.Int 10));
      BadValue (NumberType, LiteralType (Val.Str "abc"));
      BadValue (NumberType, LiteralType (Val.Str "abc"));
      BadValue (NumberType, LiteralType (Val.Str "abc"));
      BadValue (LiteralType (Val.Str "abc"), NumberType);
      BadValue (NumberType, NeverType);
    ]

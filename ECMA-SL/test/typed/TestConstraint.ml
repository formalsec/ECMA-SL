open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "examples/constraint/typeof.esl"
    [
      BadValue (FloatType, IntType);
      BadValue (IntType, FloatType);
      BadValue (BooleanType, StringType);
      BadValue (StringType, BooleanType);
      BadPossibleType (Some "baz", NullType);
      BadValue (NullType, Test.obj_cons [ ("f", IntType) ]);
    ]

let%test _ =
  Test.type_checker_test "examples/constraint/eq.esl"
    [
      BadValue (LiteralType (Val.Int 20), LiteralType (Val.Int 10));
      BadValue (LiteralType (Val.Int 10), IntType);
      BadValue (LiteralType (Val.Int 20), IntType);
      BadValue (LiteralType (Val.Bool false), LiteralType (Val.Bool true));
      BadValue (LiteralType (Val.Bool true), LiteralType (Val.Bool false));
      BadPossibleType (Some "baz", NullType);
      BadValue (NullType, Test.obj_cons [ ("f", IntType) ]);
    ]

let%test _ =
  Test.type_checker_test "examples/constraint/objectUnion.esl"
    [
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
      BadValue (IntType, StringType);
    ]

let%test _ =
  Test.type_checker_test "examples/constraint/overlap.esl"
    [
      BadValue (IntType, NeverType);
      BadValue (StringType, IntType);
      NoOverlapComp (LiteralType (Val.Int 10), LiteralType (Val.Int 20));
      NoOverlapComp (LiteralType (Val.Int 10), LiteralType (Val.Int 20));
    ]

let%test _ =
  Test.type_checker_test "examples/constraint/combine.esl"
    [
      BadValue (IntType, BooleanType);
      BadValue (StringType, IntType);
      BadValue (BooleanType, IntType);
      BadValue (IntType, StringType);
      BadValue (BooleanType, StringType);
      BadValue (LiteralType (Val.Str "abc"), LiteralType (Val.Int 10));
      BadValue (IntType, LiteralType (Val.Str "abc"));
      BadValue (IntType, LiteralType (Val.Str "abc"));
      BadValue (IntType, LiteralType (Val.Str "abc"));
      BadValue (LiteralType (Val.Str "abc"), IntType);
      BadValue (IntType, NeverType);
    ]

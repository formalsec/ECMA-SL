open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "example/match/simple.esl"
    [
      BadSigma IntType;
      BadSigma (LiteralType (Val.Int 10));
      BadSigma NullType;
      DuplicatedPatternFld "foo";
      DuplicatedPatternFld "type";
      BadLookup ("foo", Test.obj_cons [ ("type", LiteralType (Val.Str "bar")) ]);
    ]

let%test _ =
  Test.type_checker_test "example/match/discriminant.esl"
    [
      BadDiscriminant "type";
      MissingDiscriminant "type";
      UnknownDiscriminant (LiteralType (Val.Str "baz"));
    ]

let%test _ =
  Test.type_checker_test "example/match/vars.esl"
    [ BadValue (StringType, IntType); UnknownVar "foo" ]

let%test _ =
  Test.type_checker_test "example/match/cases.esl"
    [
      BadValPattern (BooleanType, LiteralType (Val.Int 10));
      BadNonePattern;
      BadValue (LiteralType (Val.Bool true), BooleanType);
      BadValue (LiteralType (Val.Bool false), BooleanType);
      UnusedPatternCase;
      BadValue (LiteralType (Val.Bool true), LiteralType (Val.Bool false));
      BadValue (UndefinedType, IntType);
      MissingPatternCase;
    ]

let%test _ =
  Test.type_checker_test "example/match/default.esl"
    [ UnusedPatternCase; UnusedPatternCase; UnusedPatternCase ]

open Ecma_sl
open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "examples/match/simple.esl"
    [
      BadSigma IntType;
      BadSigma (LiteralType (Val.Int 10));
      BadSigma NullType;
      DuplicatedPatternFld "foo";
      DuplicatedPatternFld "type";
      BadLookup ("foo", Test.obj_cons [ ("type", LiteralType (Val.Str "bar")) ]);
    ]

let%test _ =
  Test.type_checker_test "examples/match/discriminant.esl"
    [
      BadDiscriminant "type";
      MissingDiscriminant "type";
      UnknownDiscriminant (LiteralType (Val.Str "baz"));
    ]

let%test _ =
  Test.type_checker_test "examples/match/vars.esl"
    [ BadValue (StringType, IntType); UnknownVar "foo" ]

let%test _ =
  Test.type_checker_test "examples/match/cases.esl"
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
  Test.type_checker_test "examples/match/default.esl"
    [ UnusedPatternCase; UnusedPatternCase; UnusedPatternCase ]

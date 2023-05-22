open E_Type
open T_Err

let%test _ = Test.type_checker_test "example/assign/untyped.esl" []

let%test _ =
  Test.type_checker_test "example/assign/primitive.esl"
    [
      BadValue (NumberType, StringType);
      BadValue (StringType, SymbolType);
      BadValue (BooleanType, NumberType);
    ]

let%test _ =
  Test.type_checker_test "example/assign/special.esl"
    [
      BadValue (NumberType, UnknownType);
      BadValue (NeverType, NumberType);
      BadValue (SymbolType, UndefinedType);
    ]

let%test _ =
  Test.type_checker_test "example/assign/propagation.esl"
    [
      BadValue (StringType, NumberType);
      BadValue (NumberType, StringType);
      UnknownVar "baz";
    ]

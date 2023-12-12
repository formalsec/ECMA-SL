open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "examples/basic/primitive.esl"
    [
      BadValue (IntType, StringType);
      BadValue (StringType, SymbolType);
      BadValue (BooleanType, IntType);
    ]

let%test _ =
  Test.type_checker_test "examples/basic/special.esl"
    [
      BadValue (IntType, UnknownType);
      BadValue (NeverType, IntType);
      BadValue (SymbolType, UndefinedType);
    ]

let%test _ =
  Test.type_checker_test "examples/basic/propagation.esl"
    [
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
    ]

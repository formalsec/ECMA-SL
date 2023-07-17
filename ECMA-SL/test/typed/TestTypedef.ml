open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "examples/typedef/simple.esl"
    [
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      UnknownType (UserDefinedType "boolean_t");
    ]

let%test _ =
  Test.type_checker_test "examples/typedef/object.esl"
    [
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      UnknownType (UserDefinedType "fakeObject_t");
      BadValue (StringType, IntType);
    ]

let%test _ =
  Test.type_checker_test "examples/typedef/sigma.esl"
    [
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
    ]

let%test _ =
  Test.type_checker_test "examples/typedef/complex.esl"
    [
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
    ]

let%test _ =
  Test.type_checker_test "examples/typedef/recursive.esl"
    [
      BadValue (NullType, UserDefinedType "type_t");
      BadPossibleType (Some "x[bar]", NullType);
    ]

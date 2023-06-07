open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "example/object/cons.esl"
    [
      DuplicatedField "foo";
      ExtraField "bar";
      MissingField "bar";
      BadValue (StringType, BooleanType);
      MissingField "baz";
      BadValue (BooleanType, StringType);
    ]

let%test _ =
  let tobj = Test.obj_cons [ ("foo", IntType); ("bar", StringType) ] in
  Test.type_checker_test "example/object/lookup.esl"
    [
      BadValue (IntType, StringType);
      BadLookup ("baz", tobj);
      BadValue (IntType, StringType);
      BadPossibleType (Some "objComplexUndef[foo]", UndefinedType);
      BadPossibleType (Some "objUndef", UndefinedType);
      BadPossibleType (Some "objNull", NullType);
      BadType (Some "objUnknown", UnknownType);
      BadLookup ("foo", IntType);
    ]

let%test _ =
  let tobj = Test.obj_cons [ ("foo", IntType); ("bar", StringType) ] in
  Test.type_checker_test "example/object/fassign.esl"
    [
      BadValue (StringType, IntType);
      BadLookup ("baz", tobj);
      BadValue (StringType, IntType);
      BadPossibleType (Some "objComplexUndef[foo]", UndefinedType);
      BadPossibleType (Some "objUndef", UndefinedType);
      BadPossibleType (Some "objNull", NullType);
      BadType (Some "objUnknown", UnknownType);
      BadLookup ("foo", IntType);
    ]

let%test _ =
  Test.type_checker_test "example/object/narrowing.esl"
    [
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      BadValue (StringType, IntType);
      BadValue (StringType, IntType);
      BadValue (StringType, UnknownType);
    ]

let%test _ =
  Test.type_checker_test "example/object/optional.esl"
    [
      BadValue (IntType, UndefinedType);
      BadValue (StringType, UndefinedType);
      BadValue (UndefinedType, IntType);
      BadValue (UndefinedType, StringType);
    ]

let%test _ =
  Test.type_checker_test "example/object/union.esl"
    [
      BadValue (IntType, StringType);
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
      BadValue (LiteralType (Val.Int 10), IntType);
      BadValue (LiteralType (Val.Int 10), LiteralType (Val.Int 20));
    ]

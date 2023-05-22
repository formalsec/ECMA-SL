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
  let tobj = Test.obj_cons [ ("foo", NumberType); ("bar", StringType) ] in
  Test.type_checker_test "example/object/lookup.esl"
    [
      BadValue (NumberType, StringType);
      BadLookup ("baz", tobj);
      BadValue (NumberType, StringType);
      BadPossibleType (Some "obj_complex_undef[foo]", UndefinedType);
      BadPossibleType (Some "obj_undef", UndefinedType);
      BadPossibleType (Some "obj_null", NullType);
      BadType (Some "obj_unknown", UnknownType);
      BadLookup ("foo", NumberType);
    ]

let%test _ =
  let tobj = Test.obj_cons [ ("foo", NumberType); ("bar", StringType) ] in
  Test.type_checker_test "example/object/fassign.esl"
    [
      BadValue (StringType, NumberType);
      BadLookup ("baz", tobj);
      BadValue (StringType, NumberType);
      BadPossibleType (Some "obj_complex_undef[foo]", UndefinedType);
      BadPossibleType (Some "obj_undef", UndefinedType);
      BadPossibleType (Some "obj_null", NullType);
      BadType (Some "obj_unknown", UnknownType);
      BadLookup ("foo", NumberType);
    ]

let%test _ =
  Test.type_checker_test "example/object/narrowing.esl"
    [
      BadValue (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (StringType, NumberType);
      BadValue (StringType, NumberType);
      BadValue (StringType, UnknownType);
    ]

let%test _ =
  Test.type_checker_test "example/object/optional.esl"
    [
      BadValue (NumberType, UndefinedType);
      BadValue (StringType, UndefinedType);
      BadValue (UndefinedType, NumberType);
      BadValue (UndefinedType, StringType);
    ]

let%test _ =
  Test.type_checker_test "example/object/union.esl"
    [
      BadValue (NumberType, StringType);
      BadValue (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (NumberType, StringType);
      BadValue (NumberType, StringType);
      BadValue (StringType, NumberType);
      BadValue (NumberType, StringType);
      BadValue (LiteralType (Val.Int 10), NumberType);
      BadValue (LiteralType (Val.Int 10), LiteralType (Val.Int 20));
    ]

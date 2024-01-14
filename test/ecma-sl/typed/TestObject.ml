open Ecma_sl
open EType
open T_Err

let%test _ =
  Test.type_checker_test "examples/object/cons.esl"
    [ DuplicatedField "foo"
    ; ExtraField "bar"
    ; MissingField "bar"
    ; BadValue (StringType, BooleanType)
    ; MissingField "baz"
    ; BadValue (BooleanType, StringType)
    ]

let%test _ =
  let tobj = Test.obj_cons [ ("foo", IntType); ("bar", StringType) ] in
  Test.type_checker_test "examples/object/lookup.esl"
    [ BadValue (IntType, StringType)
    ; BadLookup ("baz", tobj)
    ; BadValue (IntType, StringType)
    ; BadPossibleType (Some "objComplexUndef[foo]", UndefinedType)
    ; BadPossibleType (Some "objUndef", UndefinedType)
    ; BadPossibleType (Some "objNull", NullType)
    ; BadType (Some "objUnknown", UnknownType)
    ; BadLookup ("foo", IntType)
    ]

let%test _ =
  let tobj = Test.obj_cons [ ("foo", IntType); ("bar", StringType) ] in
  Test.type_checker_test "examples/object/fassign.esl"
    [ BadValue (StringType, IntType)
    ; BadLookup ("baz", tobj)
    ; BadValue (StringType, IntType)
    ; BadPossibleType (Some "objComplexUndef[foo]", UndefinedType)
    ; BadPossibleType (Some "objUndef", UndefinedType)
    ; BadPossibleType (Some "objNull", NullType)
    ; BadType (Some "objUnknown", UnknownType)
    ; BadLookup ("foo", IntType)
    ]

let%test _ =
  Test.type_checker_test "examples/object/narrowing.esl"
    [ BadValue (IntType, StringType)
    ; BadValue (StringType, IntType)
    ; BadValue (StringType, IntType)
    ; BadValue (StringType, IntType)
    ; BadValue (StringType, UnknownType)
    ]

let%test _ =
  Test.type_checker_test "examples/object/optional.esl"
    [ BadValue (IntType, UndefinedType)
    ; BadValue (StringType, UndefinedType)
    ; BadValue (UndefinedType, IntType)
    ; BadValue (UndefinedType, StringType)
    ]

let%test _ =
  Test.type_checker_test "examples/object/union.esl"
    [ BadValue (IntType, StringType)
    ; BadValue (IntType, StringType)
    ; BadValue (StringType, IntType)
    ; BadValue (IntType, StringType)
    ; BadValue (IntType, StringType)
    ; BadValue (StringType, IntType)
    ; BadValue (IntType, StringType)
    ; BadValue (LiteralType (Val.Int 10), IntType)
    ; BadValue (LiteralType (Val.Int 10), LiteralType (Val.Int 20))
    ]

let%test _ =
  Test.type_checker_test "examples/object/sigma.esl"
    [ BadValue (LiteralType (Val.Str "bar"), LiteralType (Val.Str "foo"))
    ; BadValue (LiteralType (Val.Str "foo"), LiteralType (Val.Bool false))
    ; BadValue (LiteralType (Val.Bool false), LiteralType (Val.Str "foo"))
    ; BadLookup ("foo", Test.obj_cons [ ("type", LiteralType (Val.Bool false)) ])
    ; BadValue (LiteralType (Val.Str "foo"), LiteralType (Val.Str "bar"))
    ; BadValue (LiteralType (Val.Bool false), LiteralType (Val.Str "foo"))
    ; BadValue (LiteralType (Val.Str "foo"), LiteralType (Val.Bool false))
    ; BadLookup ("foo", Test.obj_cons [ ("type", LiteralType (Val.Bool false)) ])
    ; BadValue (LiteralType (Val.Bool false), LiteralType (Val.Str "foo"))
    ; BadValue (LiteralType (Val.Str "foo"), LiteralType (Val.Bool false))
    ]

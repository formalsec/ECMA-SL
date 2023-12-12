open Ecma_sl
open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "examples/if_while/ifEval.esl"
    [
      BadExpectedType (BooleanType, IntType);
      BadValue (StringType, IntType);
      BadValue (StringType, IntType);
    ]

let%test _ =
  Test.type_checker_test "examples/if_while/ifNarrowing.esl"
    [
      BadValue (IntType, BooleanType);
      BadValue (IntType, StringType);
      BadValue (IntType, UndefinedType);
      BadValue (IntType, BooleanType);
    ]

let%test _ =
  Test.type_checker_test "examples/if_while/whileEval.esl"
    [
      UnknownVar "test";
      BadTypeUpdate (IntType, UnionType [ IntType; StringType ]);
      BadTypeUpdate (IntType, StringType);
      BadValue (StringType, IntType);
      BadValue (StringType, IntType);
      UnknownVar "baz";
    ]

let%test _ =
  Test.type_checker_test "examples/if_while/whileNarrowing.esl"
    [
      BadValue (LiteralType (Val.Int 10), IntType);
      BadValue (LiteralType (Val.Int 20), IntType);
      BadValue
        ( UnionType [ LiteralType (Val.Int 10); LiteralType (Val.Int 20) ],
          IntType );
      BadValue
        ( UnionType [ LiteralType (Val.Int 10); LiteralType (Val.Int 20) ],
          IntType );
    ]

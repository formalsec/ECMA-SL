open Ecma_sl
open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "examples/union/simple.esl"
    [ BadValue (UnionType [ IntType; StringType ], BooleanType) ]

let%test _ =
  Test.type_checker_test "examples/union/merge.esl"
    [
      BadValue (UnionType [ BooleanType; IntType ], StringType);
      BadValue (UnionType [ IntType; StringType ], BooleanType);
    ]

let%test _ =
  Test.type_checker_test "examples/union/literal.esl"
    [
      BadValue (UnionType [ LiteralType (Val.Int 10); StringType ], IntType);
      BadValue (UnionType [ LiteralType (Val.Int 10); StringType ], BooleanType);
    ]

let%test _ =
  Test.type_checker_test "examples/union/narrowing.esl"
    [
      BadValue (StringType, IntType);
      BadValue (IntType, StringType);
      BadValue (StringType, IntType);
      BadValue (IntType, UnknownType);
    ]

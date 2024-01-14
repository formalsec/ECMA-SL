open Ecma_sl
open EType
open T_Err

let%test _ =
  Test.type_checker_test "examples/function/typedFunction.esl"
    [ DuplicatedParam "x" ]

let%test _ =
  Test.type_checker_test "examples/function/call.esl"
    [ NExpectedArgs (2, 1)
    ; NExpectedArgs (2, 3)
    ; BadArgument (IntType, StringType)
    ; UnknownFunction "bar"
    ]

let%test _ =
  Test.type_checker_test "examples/function/return.esl"
    [ BadReturn (VoidType, IntType)
    ; BadReturn (IntType, VoidType)
    ; BadReturn (StringType, IntType)
    ]

let%test _ =
  Test.type_checker_test "examples/function/operator.esl"
    [ BadOperand (BooleanType, StringType)
    ; BadOperand (IntType, FloatType)
    ; BadOperand (IntType, StringType)
    ; BadOperand (BooleanType, StringType)
    ; BadOperand (IntType, StringType)
    ]

let%test _ =
  Test.type_checker_test "examples/function/noMain.esl" [ MissingMainFunc ]

let%test _ =
  Test.type_checker_test "examples/function/badMain.esl" [ BadMainArgs ]

let%test _ =
  Test.type_checker_test "examples/function/openCodePath.esl"
    [ OpenCodePath; OpenCodePath; OpenCodePath; OpenCodePath ]

let%test _ =
  Test.type_checker_test "examples/function/unreachableCode.esl"
    [ UnreachableCode; UnreachableCode ]

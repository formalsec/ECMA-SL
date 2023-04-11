let%test _ = Test.type_checker_succ "example/func/ok_function.esl"
let%test _ = Test.type_checker_succ "example/func/ok_call.esl"

let%test _ =
  Test.type_checker_fail "example/func/nok_unknown_func.esl"
    [ T_Err.UnknownFunction "foo" ]

let%test _ =
  Test.type_checker_fail "example/func/nok_duplicate_params.esl"
    [ T_Err.DuplicatedParam "x" ]

let%test _ =
  Test.type_checker_fail "example/func/nok_missing_args.esl"
    [ T_Err.MissingArgs (2, 1) ]

let%test _ =
  Test.type_checker_fail "example/func/nok_bad_return.esl"
    [ T_Err.BadReturn (E_Type.StringType, E_Type.NumberType) ]

let%test _ =
  Test.type_checker_fail "example/func/nok_bad_arg.esl"
    [ T_Err.BadArgument (E_Type.NumberType, E_Type.StringType) ]

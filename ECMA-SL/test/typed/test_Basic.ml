let%test _ = Test.type_checker_succ "example/basic/ok_typing.esl"
let%test _ = Test.type_checker_succ "example/basic/ok_typing_propagation.esl"
let%test _ = Test.type_checker_succ "example/basic/ok_primitive.esl"
let%test _ = Test.type_checker_succ "example/basic/ok_extra_types.esl"

let%test _ =
  Test.type_checker_fail "example/basic/nok_unknown_var.esl"
    [ T_Err.UnknownVar "bar" ]

let%test _ =
  Test.type_checker_fail "example/basic/nok_bad_assignment.esl"
    [
      T_Err.BadAssignment (E_Type.StringType, E_Type.NumberType);
      T_Err.BadAssignment (E_Type.StringType, E_Type.NumberType);
    ]

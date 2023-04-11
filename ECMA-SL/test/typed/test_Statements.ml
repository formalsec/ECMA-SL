let%test _ = Test.type_checker_succ "example/stmt/ok_ifelse_intersect.esl"
let%test _ = Test.type_checker_succ "example/stmt/ok_while_intersect.esl"

let%test _ =
  Test.type_checker_fail "example/stmt/nok_ifelse_eval.esl"
    [
      T_Err.BadExpectedType (E_Type.BooleanType, E_Type.NumberType);
      T_Err.BadAssignment (E_Type.StringType, E_Type.NumberType);
      T_Err.BadAssignment (E_Type.BooleanType, E_Type.NumberType);
    ]

let%test _ =
  let tNumStr = E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ] in
  let tNumUnkn = E_Type.UnionType [ E_Type.NumberType; E_Type.UnknownType ] in
  Test.type_checker_fail "example/stmt/nok_ifelse_intersect.esl"
    [
      T_Err.BadAssignment (E_Type.NumberType, tNumStr);
      T_Err.BadAssignment (E_Type.NumberType, tNumUnkn);
      T_Err.BadAssignment (E_Type.NumberType, tNumUnkn);
    ]

let%test _ =
  let tStrNum = E_Type.UnionType [ E_Type.StringType; E_Type.NumberType ] in
  let tNumUnkn = E_Type.UnionType [ E_Type.NumberType; E_Type.UnknownType ] in
  Test.type_checker_fail "example/stmt/nok_while_intersect.esl"
    [
      T_Err.BadAssignment (E_Type.NumberType, tStrNum);
      T_Err.BadAssignment (E_Type.NumberType, tNumUnkn);
    ]

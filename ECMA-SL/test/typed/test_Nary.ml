let%test _ = Test.type_checker_succ "example/nary/ok_union.esl"

let%test _ =
  let tunion = E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ] in
  let tval = E_Type.BooleanType in
  Test.type_checker_fail "example/nary/nok_union.esl"
    [ T_Err.BadAssignment (tunion, tval) ]

let%test _ =
  let tunion = E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ] in
  let tval = E_Type.StringType in
  Test.type_checker_fail "example/nary/nok_union_simplify.esl"
    [ T_Err.BadAssignment (tval, tunion) ]

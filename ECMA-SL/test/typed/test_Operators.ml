let%test _ = Test.type_checker_succ "example/op/ok_unop.esl"
let%test _ = Test.type_checker_succ "example/op/ok_binop.esl"
let%test _ = Test.type_checker_succ "example/op/ok_triop.esl"

let%test _ =
  let targs = [ E_Type.StringType ] in
  Test.type_checker_fail "example/op/nok_neg.esl"
    [ T_Err.BadOp (Operators.str_of_unopt Operators.Neg, targs) ]

let%test _ =
  let targs = [ E_Type.NumberType; E_Type.StringType ] in
  Test.type_checker_fail "example/op/nok_plus_num_str.esl"
    [ T_Err.BadOp (Operators.str_of_binopt_single Operators.Plus, targs) ]

let%test _ =
  let targs = [ E_Type.StringType; E_Type.StringType; E_Type.NumberType ] in
  Test.type_checker_fail "example/op/nok_ssubstr.esl"
    [ T_Err.BadOp (Operators.str_of_triopt_single Operators.Ssubstr, targs) ]

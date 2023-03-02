
(* Basic Type Tests *)
let%test_unit "ok_primitive" = Test.syntax_succ "example/basic/ok_primitive.esl"
let%test_unit "ok_literal" = Test.syntax_succ "example/basic/ok_literal.esl"
let%test_unit "ok_list" = Test.syntax_succ "example/basic/ok_list.esl"

(* N-ary Type Tests *)
let%test_unit "ok_tuple" = Test.syntax_succ "example/nary/ok_tuple.esl"
let%test_unit "ok_union" = Test.syntax_succ "example/nary/ok_union.esl"
let%test_unit "ok_nary_combine" = Test.syntax_succ "example/nary/ok_nary_combine.esl"

(* Objects *)
let%test_unit "ok_object" = Test.syntax_succ "example/objects/ok_object.esl"
let%test_unit "ok_summary" = Test.syntax_succ "example/objects/ok_summary.esl"

(* Others *)
let%test_unit "ok_parenthesis" = Test.syntax_succ "example/others/ok_parenthesis.esl"
let%test_unit "ok_assignment" = Test.syntax_succ "example/others/ok_assignment.esl"
let%test_unit "ok_function" = Test.syntax_succ "example/others/ok_function.esl"
let%test_unit "ok_type_decl" = Test.syntax_succ "example/others/ok_type_decl.esl"

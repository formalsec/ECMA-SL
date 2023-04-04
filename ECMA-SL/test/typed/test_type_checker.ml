(* ======================================== Basic ======================================== *)
let%test _ = Test.type_checker_succ "example/basic/ok_typing.esl"
let%test _ = Test.type_checker_succ "example/basic/ok_typing_propagation.esl"
let%test _ = Test.type_checker_succ "example/basic/ok_primitive.esl"
let%test _ = Test.type_checker_succ "example/basic/ok_extra_types.esl"

let%test _ =
  Test.type_checker_fail "example/basic/nok_unknown_var.esl"
    [ T_Err.UnknownVar "bar" ]

let%test _ =
  Test.type_checker_fail "example/basic/nok_primitive_assignment.esl"
    [ T_Err.BadAssignment (E_Type.NumberType, E_Type.StringType) ]

let%test _ =
  Test.type_checker_fail "example/basic/nok_typing_propagation.esl"
    [ T_Err.BadAssignment (E_Type.StringType, E_Type.NumberType) ]

(* ======================================== Functions ======================================== *)
let%test _ = Test.type_checker_succ "example/function/ok_function.esl"
let%test _ = Test.type_checker_succ "example/function/ok_call.esl"
let%test _ = Test.type_checker_succ "example/function/ok_use_param.esl"

let%test _ =
  Test.type_checker_fail "example/function/nok_unknown_func.esl"
    [ T_Err.UnknownFunction "foo" ]

let%test _ =
  Test.type_checker_fail "example/function/nok_duplicate_params.esl"
    [ T_Err.DuplicatedParam "x" ]

let%test _ =
  Test.type_checker_fail "example/function/nok_return.esl"
    [ T_Err.BadReturn (E_Type.StringType, E_Type.NumberType) ]

let%test _ =
  Test.type_checker_fail "example/function/nok_missing_args.esl"
    [ T_Err.MissingArgs (2, 1) ]

let%test _ =
  Test.type_checker_fail "example/function/nok_bad_call.esl"
    [ T_Err.BadArgument (E_Type.NumberType, E_Type.StringType) ]

(* ======================================== Operators ======================================== *)
let%test _ = Test.type_checker_succ "example/op/ok_unop.esl"
let%test _ = Test.type_checker_succ "example/op/ok_binop.esl"
let%test _ = Test.type_checker_succ "example/op/ok_triop.esl"

let%test _ =
  Test.type_checker_fail "example/op/nok_neg.esl"
    [
      T_Err.BadOp (Operators.str_of_unopt Operators.Neg, [ E_Type.StringType ]);
    ]

let%test _ =
  Test.type_checker_fail "example/op/nok_plus_num_str.esl"
    [
      T_Err.BadOp
        ( Operators.str_of_binopt_single Operators.Plus,
          [ E_Type.NumberType; E_Type.StringType ] );
    ]

let%test _ =
  Test.type_checker_fail "example/op/nok_ssubstr.esl"
    [
      T_Err.BadOp
        ( Operators.str_of_triopt_single Operators.Ssubstr,
          [ E_Type.StringType; E_Type.StringType; E_Type.NumberType ] );
    ]

(* ======================================== Nary ======================================== *)
let%test _ = Test.type_checker_succ "example/nary/ok_union.esl"

let%test _ =
  Test.type_checker_fail "example/nary/nok_union.esl"
    [
      T_Err.BadAssignment
        ( E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ],
          E_Type.BooleanType );
    ]

let%test _ =
  Test.type_checker_fail "example/nary/nok_union_simplify.esl"
    [
      T_Err.BadAssignment
        ( E_Type.StringType,
          E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ] );
    ]

(* ======================================== Object ======================================== *)
let%test _ = Test.type_checker_succ "example/object/ok_object.esl"
let%test _ = Test.type_checker_succ "example/object/ok_opt_prop.esl"
let%test _ = Test.type_checker_succ "example/object/ok_lookup.esl"
let%test _ = Test.type_checker_succ "example/object/ok_fassign.esl"

let%test _ =
  Test.type_checker_fail "example/object/nok_duplicate_prop.esl"
    [ T_Err.DuplicatedField "foo" ]

let%test _ =
  Test.type_checker_fail "example/object/nok_missing_prop.esl"
    [ T_Err.MissingField "bar" ]

let%test _ =
  Test.type_checker_fail "example/object/nok_extra_prop.esl"
    [ T_Err.ExtraField "bar" ]

let%test _ =
  Test.type_checker_fail "example/object/nok_bad_prop.esl"
    [ T_Err.BadField ("bar", E_Type.StringType, E_Type.BooleanType) ]

let%test _ =
  let tobj1, tobj2, tobj3, tobj4 =
    ( Test.obj_fun [ ("foo", E_Type.NumberType); ("bar", E_Type.StringType) ],
      Test.obj_fun
        [
          ("foo", E_Type.NumberType);
          ("bar", E_Type.StringType);
          ("baz", E_Type.BooleanType);
        ],
      Test.obj_fun [ ("foo", E_Type.NumberType) ],
      Test.obj_fun [ ("foo", E_Type.NumberType); ("bar", E_Type.BooleanType) ]
    )
  in
  Test.type_checker_fail "example/object/nok_obj_ref.esl"
    [
      T_Err.BadAssignment (tobj2, tobj1);
      T_Err.BadAssignment (tobj3, tobj1);
      T_Err.BadAssignment (tobj4, tobj1);
    ]

let%test _ =
  Test.type_checker_fail "example/object/nok_lookup_no_obj.esl"
    [ T_Err.ExpectedObjectExpr (E_Expr.Var "x") ]

let%test _ =
  let tobj = Test.obj_fun [ ("foo", E_Type.NumberType) ] in
  Test.type_checker_fail "example/object/nok_lookup_missing.esl"
    [ T_Err.BadLookup (tobj, "bar") ]

let%test _ =
  Test.type_checker_fail "example/object/nok_lookup_assignment.esl"
    [ T_Err.BadAssignment (E_Type.NumberType, E_Type.StringType) ]

let%test _ =
  Test.type_checker_fail "example/object/nok_lookup_opt.esl"
    [
      T_Err.BadAssignment
        ( E_Type.StringType,
          E_Type.UnionType [ E_Type.StringType; E_Type.UndefinedType ] );
    ]

let%test _ =
  Test.type_checker_fail "example/object/nok_fassign_no_obj.esl"
    [ T_Err.ExpectedObjectExpr (E_Expr.Var "x") ]

let%test _ =
  let tobj = Test.obj_fun [ ("foo", E_Type.NumberType) ] in
  Test.type_checker_fail "example/object/nok_fassign_missing.esl"
    [ T_Err.BadLookup (tobj, "bar") ]

let%test _ =
  Test.type_checker_fail "example/object/nok_fassign.esl"
    [ T_Err.BadAssignment (E_Type.NumberType, E_Type.StringType) ]

(* ======================================== Stmts ======================================== *)
let%test _ = Test.type_checker_succ "example/stmt/ok_ifelse.esl"
let%test _ = Test.type_checker_succ "example/stmt/ok_ifelse_union.esl"
let%test _ = Test.type_checker_succ "example/stmt/ok_ifelse_union_any.esl"
let%test _ = Test.type_checker_succ "example/stmt/ok_while_union.esl"

let%test _ =
  Test.type_checker_fail "example/stmt/nok_ifelse.esl"
    [
      T_Err.BadExpectedType (E_Type.BooleanType, E_Type.NumberType);
      T_Err.BadAssignment (E_Type.StringType, E_Type.NumberType);
      T_Err.BadAssignment (E_Type.BooleanType, E_Type.NumberType);
    ]

let%test _ =
  Test.type_checker_fail "example/stmt/nok_if_unknown.esl"
    [
      T_Err.BadAssignment
        ( E_Type.NumberType,
          E_Type.UnionType [ E_Type.NumberType; E_Type.UnknownType ] );
    ]

let%test _ =
  Test.type_checker_fail "example/stmt/nok_ifelse_union.esl"
    [
      T_Err.BadAssignment
        ( E_Type.NumberType,
          E_Type.UnionType [ E_Type.NumberType; E_Type.StringType ] );
    ]

let%test _ =
  Test.type_checker_fail "example/stmt/nok_while_unknown.esl"
    [
      T_Err.BadAssignment
        ( E_Type.NumberType,
          E_Type.UnionType [ E_Type.NumberType; E_Type.UnknownType ] );
    ]

(* ======================================== Extra ======================================== *)
let%test _ = Test.type_checker_succ "example/extra/ok_gcd.esl"

let%test _ =
  Test.type_checker_fail "example/extra/nok_ifelse_return.esl"
    [ T_Err.BadReturn (E_Type.NumberType, E_Type.StringType) ]

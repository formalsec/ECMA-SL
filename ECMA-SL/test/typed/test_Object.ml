let%test _ = Test.type_checker_succ "example/obj/ok_object.esl"
let%test _ = Test.type_checker_succ "example/obj/ok_opt_prop.esl"
let%test _ = Test.type_checker_succ "example/obj/ok_lookup.esl"
let%test _ = Test.type_checker_succ "example/obj/ok_fassign.esl"

let%test _ =
  Test.type_checker_fail "example/obj/nok_prop.esl"
    [
      T_Err.DuplicatedField "foo";
      T_Err.ExtraField "bar";
      T_Err.MissingField "bar";
      T_Err.BadField ("bar", E_Type.StringType, E_Type.BooleanType);
    ]

let%test _ =
  Test.type_checker_fail "example/obj/nok_lookup.esl"
    [
      T_Err.ExpectedObjectExpr (E_Expr.Var "x");
      T_Err.BadLookup (Test.obj_fun [ ("foo", E_Type.NumberType) ], "bar");
      T_Err.BadAssignment (E_Type.NumberType, E_Type.StringType);
      T_Err.BadAssignment
        ( E_Type.StringType,
          E_Type.UnionType [ E_Type.StringType; E_Type.UndefinedType ] );
    ]

let%test _ =
  Test.type_checker_fail "example/obj/nok_fassign.esl"
    [
      T_Err.ExpectedObjectExpr (E_Expr.Var "x");
      T_Err.BadLookup (Test.obj_fun [ ("foo", E_Type.NumberType) ], "bar");
      T_Err.BadAssignment (E_Type.NumberType, E_Type.StringType);
    ]

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
  Test.type_checker_fail "example/obj/nok_obj_ref.esl"
    [
      T_Err.BadAssignment (tobj2, tobj1);
      T_Err.BadAssignment (tobj3, tobj1);
      T_Err.BadAssignment (tobj4, tobj1);
    ]

open E_Type
open T_Err

let%test _ =
  Test.type_checker_test "example/sigma/simple.esl"
    [
      BadValue (LiteralType (Val.Str "bar"), LiteralType (Val.Str "foo"));
      BadValue (LiteralType (Val.Str "foo"), LiteralType (Val.Bool false));
      BadValue (LiteralType (Val.Bool false), LiteralType (Val.Str "foo"));
      BadLookup ("foo", Test.obj_cons [ ("type", LiteralType (Val.Bool false)) ]);
      BadValue (LiteralType (Val.Str "foo"), LiteralType (Val.Str "bar"));
      BadValue (LiteralType (Val.Bool false), LiteralType (Val.Str "foo"));
      BadValue (LiteralType (Val.Str "foo"), LiteralType (Val.Bool false));
      BadLookup ("foo", Test.obj_cons [ ("type", LiteralType (Val.Bool false)) ]);
      BadValue (LiteralType (Val.Bool false), LiteralType (Val.Str "foo"));
      BadValue (LiteralType (Val.Str "foo"), LiteralType (Val.Bool false));
    ]

open Printf

module Val = struct
  include Val
  open HTMLHashtables.Val

  type ctxt_t =
    | None
    | NoMatch
    | Negative

  let rec to_html ?(ctxt=None) (v : t) : string =
    let wrap_in_b (s : string) =
      sprintf "<b>%s</b>" s in
    match v with
    | Void     -> ""
    | Flt _    ->
      let flt = Val.str ~flt_with_dot:false v in
      (match ctxt, flt with
       | Negative, "0"   -> "<span class=\"symbol\"><b>−</b></span>" ^ (wrap_in_b "0")
       | None, "0"       -> wrap_in_b "+0"
       | None, "nan"     -> wrap_in_b "NaN"
       | Negative, "inf" -> "<span class=\"symbol\"><b>−</b></span>" ^ (wrap_in_b "Infinity")
       | None, "inf"     -> wrap_in_b "+Infinity"
       | Negative, _     -> "- " ^ wrap_in_b flt
       | _               -> wrap_in_b flt)
    | Int _
    | Bool _   -> wrap_in_b (Val.str v)
    | Str v when ctxt = NoMatch ->
      sprintf
        "\"<code>%s</code>\""
        (wrap_in_b v)
    | Str v    ->
      (match Hashtbl.find_opt val_hashtable_html v with
       | None   ->
         sprintf
           "\"<code>%s</code>\""
           (wrap_in_b v)
       | Some s -> s)
    | Null -> ""
    | Symbol s -> wrap_in_b s
    | _        -> invalid_arg ("Invalid argument passed to Val.to_html: " ^ str v)
end

module E_Expr = struct
  include E_Expr
  open HTMLHashtables.E_Expr

  type ctxt_t =
    | BinaryExpr
    | Call
    | IfGuard
    | Set
    | Let
    | Negative
    | Assert
    | AssertNegative
    | ExprStmt
    | Table
    | TopLevel

  let lookup_hashtable_html = Hashtbl.create 0
  let call_hashtable_html = Hashtbl.create 0
  let unoper_hashtable_html = Hashtbl.create 0
  let binoper_hashtable_html = Hashtbl.create 0

  let rec to_html (ctxt : ctxt_t) (e : t) : string =
    let aux = to_html ctxt in
    match e with
    | GVar v ->
      (match v with
       | "global" -> "the <a href=\"#sec-15.1\">global object</a>"
       | _        -> "")
    | Const _ -> ""
    | Val v ->
      (match ctxt, v with
       | Call, Str s     -> s
       | Negative, Flt _ -> Val.to_html ~ctxt:Negative v
       | _               -> Val.to_html v)
    | Var x ->
      let x' = Hashtbl.find_opt vars_hashtable_html x in
      (match ctxt, x' with
       | _, Some s      -> s
       | Negative, None ->
         sprintf
           "the result of negating <i>%s</i>"
           x
       | _, None        -> "<i>" ^ x ^ "</i>")
    | UnOpt (op, e) ->
      let op_str = Oper.str_of_unopt op in
      let f_op = Hashtbl.find_opt unoper_hashtable_html op_str in
      let res_op = Option.map_default (fun f -> f ctxt op_str e (Val Null)) None f_op in
      (match res_op with
       | Some s -> s
       | None ->
         let e_html = to_html ctxt e in
         sprintf "%s %s" op_str e_html)
    | BinOpt (op, e1, e2) ->
      let op_str = Oper.str_of_binopt_single op in
      let f_op = Hashtbl.find_opt binoper_hashtable_html op_str in
      let res_op = Option.map_default (fun f -> f ctxt op_str e1 e2) None f_op in
      (match res_op with
       | Some s -> s
       | None ->
         let e1_html = to_html BinaryExpr e1 in
         let e2_html = to_html BinaryExpr e2 in
         sprintf "%s %s %s" e1_html op_str e2_html)
    | EBinOpt (op, e1, e2) ->
      let op_str = EOper.str_of_binopt_single op in
      let f_op = Hashtbl.find_opt binoper_hashtable_html op_str in
      let res_op = Option.map_default (fun f -> f ctxt op_str e1 e2) None f_op in
      (match res_op with
       | Some s -> s
       | None ->
         let e1_html = to_html BinaryExpr e1 in
         let e2_html = to_html BinaryExpr e2 in
         sprintf "%s %s %s" e1_html op_str e2_html)
    | NOpt (op, es) ->
      (match op, es with
       | ListExpr, [] -> "an empty <a href=\"#sec-8.8\">List</a>"
       | ListExpr, [e1; e2] ->
         sprintf
           "the pair (a two element List) consisting of %s and %s"
           (to_html ctxt e1)
           (to_html ctxt e2)
       | _, _ -> invalid_arg ("Invalid n-ary operator passed to E_Expr.to_html: " ^ (Oper.str_of_nopt op (List.map str es)))
      )
    | Call (f, es, _) ->
      (match ctxt with
       | IfGuard ->
         let f' =
           match f with
           | Val (Str f) -> f
           | _           -> aux f in
         let f_f' = Hashtbl.find_opt call_hashtable_html f' in
         let res_f' = Option.map_default (fun fn -> fn ctxt f es) None f_f' in
         (match res_f' with
          | Some s -> s
          | None ->
            sprintf
              "%s(%s)"
              (aux f)
              (String.concat ", " (List.map aux es))
         )
       | _ ->
         let f' = to_html Call f in
         let f_f' = Hashtbl.find_opt call_hashtable_html f' in
         let res_f' = Option.map_default (fun fn -> fn ctxt f es) None f_f' in
         match res_f' with
         | Some s -> s
         | None   ->
           let args_str = (
             match es with
             | []      -> "no arguments"
             | e :: [] -> (aux e) ^ " as argument"
             | es'     ->
               let rev_es = List.rev es' in
               let last_e = List.hd rev_es in
               let rest_es = List.tl rev_es in
               sprintf
                 "%s, and %s as arguments"
                 (String.concat ", " (List.rev_map aux rest_es))
                 (aux last_e)
           ) in
           sprintf
             "the result of calling %s passing %s"
             f'
             args_str
      )
    | Lookup (e, f) ->
      let f' = str f in
      (match ctxt with
       | Call   -> f'
       | _      ->
         let f'' = Hashtbl.find_opt lookup_hashtable_html f' in
         (match f'' with
          | None    ->
            sprintf
              "the %s property of %s"
              (aux f)
              (aux e)
          | Some fn -> fn ctxt e f)
      )
    | NewObj fes ->
      sprintf "{%s}"
        (String.concat ", "
           (List.map (
               fun (f, e) ->
                 sprintf "%s: %s"
                   (aux (Val (Str f)))
                   (aux e))
               fes))
    | _ -> invalid_arg ("Unexpected argument passed to E_Expr.to_html: " ^ str e)


  let lookup_html_ownproperty (ctxt : ctxt_t) (e : t) (f : t) : string =
    let e_html = to_html ctxt e in
    let f_html = to_html ctxt f in
    sprintf
      "%s's own property named %s"
      e_html
      f_html

  let lookup_html_attribute (attribute_name : string) (ctxt : ctxt_t) (e : t) (f : t) : string =
    let e_html = to_html ctxt e in
    match attribute_name, ctxt with
    | _, Set ->
      sprintf
        "%s's [[%s]] attribute"
        e_html
        attribute_name
    | "Get", _ | "Put", _ ->
      sprintf
        "the [[%s]] internal method of %s"
        attribute_name
        e_html
    | _ ->
      sprintf
        "%s.[[%s]]"
        e_html
        attribute_name

  let lookup_html_internal_property (property_name : string) (ctxt : ctxt_t) (e : t) (f : t) : string =
    let e_html = to_html ctxt e in
    sprintf
      "the value of the [[%s]] internal property of %s"
      property_name
      e_html

  let call_html_property_descriptor (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "the <a href=\"#sec-8.10\">Property Descriptor</a> %s"
          (to_html TopLevel e))
    | _ -> None

  let call_html_frompropertydescriptor (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the result of calling <a href=\"#sec-8.10.4\">FromPropertyDescriptor</a>(%s) (<a href=\"#sec-8.10.4\">8.10.4</a>)"
          (to_html ctxt e1))
    | _ -> None

  let call_html_topropertydescriptor (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the result of calling <a href=\"#sec-8.10.5\">ToPropertyDescriptor</a> with %s as the argument"
          (to_html ctxt e1))
    | _ -> None


  let call_html_type (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match ctxt, es with
    | Table, [ Var v ] ->
      Some (
        sprintf
          "%s Type"
          (String.capitalize_ascii v)
      )
    | _, [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-8\">Type</a>(%s)"
          (to_html TopLevel e))
    | _ -> None

  let call_html_conversion_abstract_operations (oper_name : string) (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match oper_name, es with
    | "ToPrimitive", [ arg; Val Null ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.1\">ToPrimitive</a>(%s)"
          (to_html TopLevel arg))
    | "ToPrimitive", [ e1; e2 ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.1\">ToPrimitive</a>(%s, hint %s)"
          (to_html TopLevel e1)
          (to_html TopLevel e2))
    | "ToBoolean", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.2\">ToBoolean</a>(%s)"
          (to_html TopLevel e))
    | "ToNumber", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.3\">ToNumber</a>(%s)"
          (to_html TopLevel e))
    | "ToInteger", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.4\">ToInteger</a>(%s)"
          (to_html TopLevel e))
    | "ToInt32", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.5\">ToInt32</a>(%s)"
          (to_html TopLevel e))
    | "ToUint32", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.6\">ToUint32</a>(%s)"
          (to_html TopLevel e))
    | "ToUint16", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.7\">ToUint16</a>(%s)"
          (to_html TopLevel e))
    | "ToString", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.8\">ToString</a>(%s)"
          (to_html TopLevel e))

    | "ToObject", [ Var "this" ] -> None
    | "ToObject", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-9.9\">ToObject</a>(%s)"
          (to_html TopLevel e))
    | "CheckObjectCoercible", [ e ] ->
      Some (
        sprintf
          "Call <a href=\"#sec-9.10\">CheckObjectCoercible</a>(%s)"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_ownproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    let obj = List.nth es 0 in
    let field = List.nth es 1 in
    Some (
      sprintf
        "%s's own property named %s"
        (to_html TopLevel obj)
        (to_html TopLevel field)
    )

  let call_html_internal_method ?(print_brackets=true) (method_name : string) (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match ctxt, method_name, es with
    | ExprStmt, "put", [ e_this; e_prop; e_value; e_strict ] ->
      Some (
        sprintf
          "Call the <i>put</i> internal method using %s as its <b>this</b> value, and passing %s for the property name, %s for the value, and %s for the <i>Throw</i> flag"
          (to_html TopLevel e_this)
          (to_html TopLevel e_prop)
          (to_html TopLevel e_value)
          (to_html TopLevel e_strict)
      )
    | ExprStmt, "Put", [ e_o; e_prop; e_value; e_strict ] ->
      Some (
        sprintf
          "Call the [[Put]] internal method of %s, passing %s for the property name, %s for the value, and %s for the <i>Throw</i> flag"
          (to_html TopLevel e_o)
          (to_html TopLevel e_prop)
          (to_html TopLevel e_value)
          (to_html TopLevel e_strict)
      )
    | _, "get", [ e_this; e_prop ] ->
      Some (
        sprintf
          "the result of calling the <i>get</i> internal method using %s as its <b>this</b> value, and passing %s for the argument"
          (to_html TopLevel e_this)
          (to_html TopLevel e_prop)
      )
    | _, "Construct", [ _; _; e_o; e_p ] ->
      Some (
        sprintf
          "the result of calling the [[Construct]] internal method of %s providing %s as the arguments"
          (to_html TopLevel e_o)
          (to_html TopLevel e_p)
      )
    | ExprStmt, _, [ e_o; e_f ] ->
      Some (
        sprintf
          "Call the %s internal method of %s passing %s as the argument"
          (if print_brackets then "[[" ^ method_name ^ "]]" else "<i>" ^ method_name ^ "</i>")
          (to_html TopLevel e_o)
          (to_html TopLevel e_f)
      )
    | ExprStmt, _, [ e_o; e_f1; e_f2 ] ->
      Some (
        sprintf
          "Call the %s internal method of %s passing %s, and %s as arguments"
          (if print_brackets then "[[" ^ method_name ^ "]]" else "<i>" ^ method_name ^ "</i>")
          (to_html TopLevel e_o)
          (to_html TopLevel e_f1)
          (to_html TopLevel e_f2)
      )
    | ExprStmt, _, [ e_o; e_f1; e_f2; e_f3 ] ->
      Some (
        sprintf
          "Call the %s internal method of %s passing %s, %s, and %s as arguments"
          (if print_brackets then "[[" ^ method_name ^ "]]" else "<i>" ^ method_name ^ "</i>")
          (to_html TopLevel e_o)
          (to_html TopLevel e_f1)
          (to_html TopLevel e_f2)
          (to_html TopLevel e_f3)
      )
    | _, _, [ e_o; e_f] ->
      Some(
        sprintf
          "the result of calling the %s internal method of %s with argument %s"
          (if print_brackets then "[[" ^ method_name ^ "]]" else "<i>" ^ method_name ^ "</i>")
          (to_html TopLevel e_o)
          (to_html TopLevel e_f)
      )
    | _, _, [ e_o; e_f1; e_f2] ->
      Some(
        sprintf
          "the result of calling the %s internal method of %s with arguments %s and %s"
          (if print_brackets then "[[" ^ method_name ^ "]]" else "<i>" ^ method_name ^ "</i>")
          (to_html TopLevel e_o)
          (to_html TopLevel e_f1)
          (to_html TopLevel e_f2)
      )
    | _ -> None

  let call_html_custom_method (method_name : string) (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match method_name, ctxt, es with
    | "DefaultValue", Table, [ e1; e2] ->
      Some (
        sprintf
          "Return a default value for the Object. The default value of an object is retrieved by calling the [[DefaultValue]] internal method of the object, passing the optional hint %s.
          The behaviour of the [[DefaultValue]] internal method is defined by this specification for all native ECMAScript objects in <a href=\"x8.12.8\">8.12.8</a>."
          (to_html TopLevel e2)
      )
    | _ -> None

  let call_html_default_method (method_name : string) (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match method_name, es with
    | "Get", [ e1; e2 ] ->
      Some (
        sprintf
          "the result of calling the default [[%s]] internal method (<a href=\"#sec-8.12.3\">8.12.3</a>) on the %s passing %s as the argument"
          method_name
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | "GetOwnProperty", [ e1; e2 ] ->
      Some (
        sprintf
          "the result of calling the default [[%s]] internal method (<a href=\"#sec-8.12.1\">8.12.1</a>) on the %s passing %s as the argument"
          method_name
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | "DefineOwnProperty", [e1; e2; e3; e4] ->
      Some (
        sprintf
          "the result of calling the default [[%s]] internal method (<a href=\"#sec-8.12.9\">8.12.9</a>) on the %s passing %s, %s, and %s as the arguments"
          method_name
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
          (to_html ctxt e4)
      )

    | "Delete", [e1; e2; e3] ->
      Some (
        sprintf
          "the result of calling the default [[%s]] internal method (<a href=\"#sec-8.12.9\">8.12.9</a>) on the %s passing %s and %s as the arguments."
          method_name
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
      )
    | _ -> None


  let call_html_call (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match ctxt, es with
    | ExprStmt, [Val Null; Val Null; e_o; e_this; NOpt (Oper.ListExpr, [])] ->
      Some (
        sprintf
          "Call the [[Call]] internal method of %s providing %s as the <b>this</b> value and providing no arguments"
          (to_html TopLevel e_o)
          (to_html TopLevel e_this))
    | ExprStmt, [Val Null; Val Null; e_o; e_this; NOpt (Oper.ListExpr, [ e ])] ->
      Some (
        sprintf
          "Call the [[Call]] internal method of %s providing %s as the <b>this</b> value and an argument list containing only %s"
          (to_html TopLevel e_o)
          (to_html TopLevel e_this)
          (to_html TopLevel e))
    | Let, [Val Null; Val Null; e_o; e_this; NOpt (Oper.ListExpr, [])] ->
      Some (
        sprintf
          "the result of calling the [[Call]] internal method of %s, with %s as the this value and an empty argument list"
          (to_html TopLevel e_o)
          (to_html TopLevel e_this))
    | Let, [_; _; e_o; e_this; e_args] ->
      Some (
        sprintf
          "the result of calling the [[Call]] internal property of %s, providing %s as the <b>this</b> value and providing the %s as <i>args</i>"
          (to_html TopLevel e_o)
          (to_html TopLevel e_this)
          (to_html TopLevel e_args)
      )
    | _, [Val Null; Val Null; e_o; e_this; NOpt (Oper.ListExpr, [])] ->
      Some (
        sprintf
          "the result of calling the [[Call]] internal method of %s providing %s as the <b>this</b> value and providing no arguments"
          (to_html TopLevel e_o)
          (to_html TopLevel e_this))
    | _, [Val Null; Val Null; e_o; e_this; NOpt (Oper.ListExpr, [ e ])] ->
      Some (
        sprintf
          "the result of calling the [[Call]] internal method of %s providing %s as the <b>this</b> value and an argument list containing only %s"
          (to_html TopLevel e_o)
          (to_html TopLevel e_this)
          (to_html TopLevel e))
    | _, [Val Null; Val Null; e_o; e_this; NOpt (Oper.ListExpr, arr)] ->
      Some (
        sprintf
          "the result of calling the [[Call]] internal method of %s providing %s as the <b>this</b> value and an argument list containing %s"
          (to_html TopLevel e_o)
          (to_html TopLevel e_this)
          (String.concat ", " (List.map (to_html TopLevel) arr)))
    | _, [_; _; e_o; e_this; args_list] ->
      Some (
        sprintf
          "the result of calling the [[Call]] internal method of %s providing %s as the <b>this</b> value and %s as the list of arguments"
          (to_html TopLevel e_o)
          (to_html TopLevel e_this)
          (to_html TopLevel args_list))
    | _ -> None

  let call_html_getbindingvalue_method (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2; e3] ->
      Some (
        sprintf
          "the result of calling the GetBindingValue (see <a href=\"#10.2.1\">10.2.1</a>) concrete method of %s passing %s and %s as arguments"
          (to_html TopLevel e1)
          (to_html TopLevel e2)
          (to_html TopLevel e3)
      )
    | _ -> None

  let call_html_hasbinding_method (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "the result of calling %s’s HasBinding concrete method passing %s as the argument"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_deletebinding_method (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "the result of calling the DeleteBinding concrete method of %s, providing %s as the argument"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_createmutablebinding_method (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; Val (Str s); Val (Bool false)] ->
      Some (
        sprintf
          "Call %s’s CreateMutableBinding concrete method passing the String %s as the argument"
          (to_html ctxt e1)
          (to_html ctxt (Val (Str s)))
      )
    | [e1; e2; Val (Bool false)] ->
      Some (
        sprintf
          "Call %s’s CreateMutableBinding concrete method passing %s as the argument"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | [e1; e2; e3] ->
      Some (
        sprintf
          "Call %s’s CreateMutableBinding concrete method passing %s and %s as the arguments"
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
      )
    | _ -> None

  let call_html_createimmutablebinding_method (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; Val (Str s)] ->
      Some (
        sprintf
          "Call %s’s <a href=\"x10.2.1.1.7\">CreateImmutableBinding</a> concrete method passing the String %s as the argument"
          (to_html ctxt e1)
          (to_html ctxt (Val (Str s)))
      )
    | [e1; e2] ->
      Some (
        sprintf
          "Call %s’s <a href=\"x10.2.1.1.7\">CreateImmutableBinding</a> concrete method passing %s as the argument"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_initializeimmutablebinding_method (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2; e3] ->
      Some (
        sprintf
          "Call %s’s <a href=\"x10.2.1.1.8\">InitializeImmutableBinding</a> concrete method passing %s and %s as arguments"
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
      )
    | _ -> None

  let call_html_setmutablebinding (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2; e3; e4] ->
      Some (
        sprintf
          "Call the SetMutableBinding (<a href=\"#sec-10.2.1\">10.2.1</a>) concrete method of %s, passing %s, %s, and %s as arguments"
          (to_html TopLevel e1)
          (to_html TopLevel e2)
          (to_html TopLevel e3)
          (to_html TopLevel e4)
      )
    | _ -> None

  let call_html_implicitthisvalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the result of calling the ImplicitThisValue concrete method of %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_createargumentsobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2; e3; e4; e5; _] ->
      Some (
        sprintf
          "the result of calling the abstract operation CreateArgumentsObject (<a href=\"#sec-10.6\">10.6</a>) passing %s, %s, %s, %s and %s as arguments."
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
          (to_html ctxt e4)
          (to_html ctxt e5)
      )
    | _ -> None

  let call_html_evaluating (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; Var "scope" ] ->
      Some (
        sprintf
          "the result of evaluating %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_evaluatingarguments (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; Var "scope" ] ->
      Some (
        sprintf
          "the result of evaluating %s, producing an internal list of argument values (<a href=\"#sec-11.2.4\">see 11.2.4</a>)"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_functiondeclaration (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "the result of instantiating <>FunctionDeclaration f</> as described in <a href=\"#sec-13\">Clause 13</a>"
    )

  let call_html_functionbody (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; _] ->
      Some (
        sprintf
          "the result of evaluating the <i><a href=\"#sec-13\">FunctionBody</a></i> that is %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_abstractrelationalcomparison (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2; Val (Bool true) ] ->
      Some (
        sprintf
          "the result of performing abstract relational comparison %s &lt; %s. (<a href=\"#sec-11.8.5\">see 11.8.5</a>)"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | [ e1; e2; Val (Bool false) ] ->
      Some (
        sprintf
          "the result of performing abstract relational comparison %s &lt; %s with <i>LeftFirst</i> equal to <b>false</b>. (<a href=\"#sec-11.8.5\">see 11.8.5</a>)"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_abstractequalitycomparison (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "the result of performing abstract equality comparison %s == %s. (<a href=\"#sec-11.9.3\">see 11.9.3</a>)"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_strictequalitycomparison (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "the result of performing the strict equality comparison %s === %s. (See <a href=\"#sec-11.9.6\">11.9.6</a>)"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_getidentifierfunctiondeclaration (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ f ] ->
      Some (
        sprintf
          "the <i>Identifier</i> in <i><a href=\"\">FunctionDeclaration</a></i> %s"
          (to_html ctxt f)
      )
    | _ -> None

  let call_html_getidentifiervariabledeclaration (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ f ] ->
      Some (
        sprintf
          "the <i>Identifier</i> in %s"
          (to_html ctxt f)
      )
    | _ -> None

  let call_html_getstringvalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ f ] ->
      Some (
        sprintf
          "the String value that is the name of %s"
          (to_html ctxt f)
      )
    | _ -> None

  let call_html_constructor (constructor : string) (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match ctxt, es with
    | Table, [ GVar _; _; _; NOpt (Oper.ListExpr, [ expr ]) ] ->
      let built_in, section_html =
        (match e with
         | Val (Str "BooleanConstructor") -> "Boolean", "<a href=\"#sec-15.6\">15.6</a>"
         | Val (Str "NumberConstructor")  -> "Number", "<a href=\"#sec-15.7\">15.7</a>"
         | Val (Str "StringConstructor")  -> "String", "<a href=\"#sec-15.5\">15.5</a>"
         | _                              -> invalid_arg ("Unexpected expression: " ^ (str e))) in
      Some (
        sprintf
          "Create a new %s object whose [[PrimitiveValue]] internal property is set to the value of the argument.
          See %s for a description of %s objects."
          built_in
          section_html
          built_in
      )
    | _, [ GVar _; Val (Symbol "null"); _; NOpt (Oper.ListExpr, [ Val Null ]) ]
    | _, [ _; _; _; NOpt (Oper.ListExpr, [ ]) ] ->
      let const_hmtl =
        (match e with
         | Val (Str "ObjectConstructor") -> "<a href=\"#sec-15.2\">the standard built-in constructor with that name</a>"
         | _                             -> "the standard built-in constructor with that name"
        ) in
      Some (
        sprintf
          "the result of creating a new object as if by the expression <code><b>new %s()</b></code> where <code><b>%s</b></code> is %s"
          constructor
          constructor
          const_hmtl
      )
    | _, [ _; _; _; NOpt (Oper.ListExpr, [ e1 ]) ] ->
      let const_hmtl =
        (match e with
         | Val (Str "ObjectConstructor") -> "<a href=\"#sec-15.2\">the standard built-in constructor with that name</a>"
         | _                             -> "the standard built-in constructor with that name"
        ) in
      Some (
        sprintf
          "the result of creating a new object as if by the expression <code><b>new %s(%s)</b></code> where <code><b>%s</b></code> is %s"
          constructor
          (to_html ctxt e1)
          constructor
          const_hmtl
      )
    | _ -> None

  let call_html_objectdefineproperties_builtin_method (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [_; _; _; NOpt (Oper.ListExpr, [e1; e2])] ->
      Some (
        sprintf
          "Add own properties to %s as if by calling the standard built-in function <code><b>Object.defineProperties</b></code> with arguments %s and %s"
          (to_html ctxt e1)
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None


  let call_html_isprimitivevalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es, ctxt with
    | [ e ], _ ->
      Some (
        sprintf
          "%s is a <a href=\"#primitive_value\">primitive value</a>"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_iscallable (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es, ctxt with
    | [ e ], BinaryExpr ->
      Some (
        sprintf
          "<a href=\"#sec-9.11\">IsCallable</a>(%s)"
          (to_html TopLevel e)
      )
    | [ e ], _ ->
      Some (
        sprintf
          "%s is callable"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_reference_abstract_operations (oper_name : string) (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    let wrap_in_8_7_link (text : string) =
      sprintf
        "<a href=\"#sec-8.7\">%s</a>"
        text in

    match ctxt, oper_name, es with
    | ExprStmt, "GetValue", [ e ] ->
      Some (
        sprintf
          "Call <a href=\"#sec-8.7.1\">GetValue</a>(%s)"
          (to_html TopLevel e)
      )
    | _, "GetValue", [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-8.7.1\">GetValue</a>(%s)"
          (to_html TopLevel e)
      )
    | _, "PutValue", [ e1; e2 ] ->
      Some (
        sprintf
          "Call <a href=\"#sec-8.7.2\">PutValue</a>(%s, %s)"
          (to_html TopLevel e1)
          (to_html TopLevel e2)
      )
    | _, _, [ e ] ->
      Some (
        sprintf
          "%s(%s)"
          (wrap_in_8_7_link oper_name)
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_dataproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es, ctxt with
    | [ e ], BinaryExpr ->
      Some (
        sprintf
          "<a href=\"#sec-8.10.2\">IsDataDescriptor</a>(%s)"
          (to_html TopLevel e)
      )
    | [ e ], _ ->
      Some (
        sprintf
          "%s is a data property"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_accessorproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es, ctxt with
    | [ e ], BinaryExpr ->
      Some (
        sprintf
          "<a href=\"#sec-8.10.1\">IsAccessorDescriptor</a>(%s)"
          (to_html TopLevel e)
      )
    | [ e ], _ ->
      Some (
        sprintf
          "%s is an accessor property"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_genericproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "<a href=\"#sec-8.10.3\">IsGenericDescriptor</a>(%s)"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_samevalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "<a href=\"\">SameValue</a>(%s, %s)"
          (to_html TopLevel e1)
          (to_html TopLevel e2)
      )
    | _ -> None

  let call_html_dataproperty_to_accessorproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ o; f ] ->
      Some (
        sprintf
          "Convert the property named %s of object %s from a data property to an accessor property.
          Preserve the existing values of the converted property’s [[Configurable]] and [[Enumerable]] attributes and
          set the rest of the property’s attributes to their default values"
          (to_html TopLevel f)
          (to_html TopLevel o)
      )
    | _ -> None

  let call_html_accessorproperty_to_dataproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ o; f ] ->
      Some (
        sprintf
          "Convert the property named %s of object %s from an accessor property to a data property.
          Preserve the existing values of the converted property’s [[Configurable]] and [[Enumerable]] attributes and
          set the rest of the property’s attributes to their default values"
          (to_html TopLevel f)
          (to_html TopLevel o)
      )
    | _ -> None

  let call_html_createownaccessorproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ o; f; desc ] ->
      Some (
        sprintf
          "Create an own accessor property named %s of object %s whose [[Get]], [[Set]], [[Enumerable]] and [[Configurable]] attribute values are described by %s.
          If the value of an attribute field of %s is absent, the attribute of the newly created property is set to its default value"
          (to_html TopLevel f)
          (to_html TopLevel o)
          (to_html TopLevel desc)
          (to_html TopLevel desc)
      )
    | _ -> None

  let call_html_createowndataproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ o; f; desc ] ->
      Some (
        sprintf
          "Create an own data property named %s of object %s whose [[Value]], [[Writable]], [[Enumerable]] and [[Configurable]] attribute values are described by %s.
          If the value of an attribute field of %s is absent, the attribute of the newly created property is set to its default value"
          (to_html TopLevel f)
          (to_html TopLevel o)
          (to_html TopLevel desc)
          (to_html TopLevel desc)
      )
    | _ -> None

  let call_html_setcorrespondinglynamedattributes (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ o; f; desc ] ->
      Some (
        sprintf
          "For each attribute field of %s that is present, set the correspondingly named attribute of the property named %s of object %s to the value of the field"
          (to_html TopLevel desc)
          (to_html TopLevel f)
          (to_html TopLevel o)
      )
    | _ -> None

  let call_html_everyfieldindescalsooccursincurrent (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "every field in %s also occurs in %s and the value of every field in %s is the same value as the corresponding field in %s when compared using the <a href=\"#sec-9.12\">SameValue</a> algorithm (<a href=\"#sec-9.12\">9.12</a>)"
          (to_html TopLevel e1)
          (to_html TopLevel e2)
          (to_html TopLevel e1)
          (to_html TopLevel e2)
      )
    | _ -> None

  let call_html_createmutablebinding (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2; e3] ->
      Some (
        sprintf
          "Create a mutable binding in %s for %s and set its bound value to %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
      )
    | _ -> None

  let call_html_setbindingdeletable (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      sprintf
        "record that the newly created binding may be deleted by a subsequent DeleteBinding call"
    )

  let call_html_ismutablebinding (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "the binding for %s in %s is a mutable binding"
          (to_html ctxt e2)
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_setbindingvalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2; e3] ->
      Some (
        sprintf
          "Set the bound value for %s in %s to %s"
          (to_html ctxt e2)
          (to_html ctxt e1)
          (to_html ctxt e3)
      )
    | _ -> None

  let call_html_getbindingvalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "the value currently bound to %s in %s"
          (to_html ctxt e2)
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_isuninitialisedbinding (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "the binding for %s in %s is an uninitialised <a href=\"#immutable-binding\">immutable binding</a>"
          (to_html ctxt e2)
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_isbindingcannotbedeleted (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "the binding for %s in %s cannot be deleted"
          (to_html ctxt e2)
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_hasuninitialisedimmutablebinding (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "%s must have an uninitialized <a href=\"#immutable-binding\">immutable binding</a> for %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_createimmutablebinding (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "Create an <a href=\"#immutable-binding\">immutable binding</a> in %s for %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_setbindinginitialised (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2; Val (Bool false)] ->
      Some (
        sprintf
          "Record that the binding for %s in %s is uninitialised"
          (to_html ctxt e2)
          (to_html ctxt e1)
      )
    | [e1; e2; Val (Bool true)] ->
      Some (
        sprintf
          "Record that the <a href=\"#immutable-binding\">immutable binding</a> for %s in %s has been initialised"
          (to_html ctxt e2)
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_getbindingobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "the <a href=\"x10.2.1.2\">binding object</a> for %s"
          (to_html ctxt e)
      )
    | _ -> None

  let call_html_getprovidethis (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "the <i>provideThis</i> flag of %s"
          (to_html ctxt e)
      )
    | _ -> None

  let call_html_getthisbinding (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "the same value as the ThisBinding of the calling execution context"
    )

  let call_html_newvaluereference (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2; e3 ] ->
      Some (
        sprintf
          "a value of type <a href=\"#sec-8.7\">Reference</a> whose base value is %s, whose referenced name is %s, and whose strict mode flag is %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
      )
    | _ -> None

  let call_html_getenvironmentrecord (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "%s‘s environment record"
          (to_html ctxt e)
      )
    | _ -> None

  let call_html_getenvrecofrunningexecctx (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "the <a href=\"#sec-10.2\">environment record</a> component of the running execution context’s VariableEnvironment"
    )

  let call_html_setenvironmentrecord (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "Set %s’s environment record to be %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_getouterenvironmentreference (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "the value of %s’s <a href=\"#outer-environment-reference\">outer environment reference</a>"
          (to_html ctxt e)
      )
    | _ -> None

  let call_html_setouterlexicalenvironmentreference (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "Set the outer lexical environment reference of %s to %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_newlexicalenvironment (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some "a new <a href=\"#sec-10.2\">Lexical Environment</a>"

  let call_html_getglobalenvironment (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some "the <a href=\"#sec-10.2.3\">Global Environment</a>"

  let call_html_setlexicalenvironment (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ _; e1 ] ->
      Some (
        sprintf
          "Set the LexicalEnvironment to %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_getlexicalenvironment (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ Var "callingExecCtx" ] ->
      Some (
        "the same value as the <a href=\"#sec-10.2\">LexicalEnvironment</a> of the calling execution context"
      )
    | [ Var "runningExecCtx" ] ->
      Some (
        "the running execution context‘s <a href=\"#sec-10.2\">LexicalEnvironment</a>"
      )
    | _ ->
      Some (
        "the LexicalEnvironment"
      )

  let call_html_setvariableenvironment (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ _; e1 ] ->
      Some (
        sprintf
          "Set the VariableEnvironment to %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_getvariableenvironment (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ Var "callingExecCtx" ] ->
      Some (
        "the same value as the VariableEnvironment of the calling execution context"
      )
    | _ -> None

  let call_html_newdeclarativeenvironment (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the result of calling <a href=\"#sec-10.2.2.2\">NewDeclarativeEnvironment</a> passing %s as the argument"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_newdeclarativeenvironmentrecord (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some "a new <a href=\"#declarative-environment-record\">declarative environment record</a> containing no bindings"

  let call_html_newobjectenvironmentrecord (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "a new <a href=\"#object-environment-record\">object environment record</a> containing %s as the <a href=\"x10.2.1.2\">binding object</a>"
          (to_html ctxt e)
      )
    | _ -> None

  let call_html_declarationbindinginstantiation (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [Var x; _; _; Val Null] ->
      Some (
        sprintf
          "Perform Declaration Binding Instantiation as described in <a href=\"#sec-10.5\">10.5</a> using the %s"
          (to_html ctxt (Var x))
      )
    | [e1; e2; _; _] ->
      Some (
        sprintf
          "Perform Declaration Binding Instantiation using the <a href=\"#function-code\">function code</a> %s and %s as described in <a href=\"#sec-10.5\">10.5</a>"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_makearggetter (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "the result of calling the <i><a href=\"#MakeArgGetter\">MakeArgGetter</a></i> abstract operation with arguments %s and %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_makearggetterletbodyauxfunction (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the result of concatenating the Strings %s, %s, and %s"
          (to_html ctxt (Val (Str "return ")))
          (to_html ctxt e1)
          (to_html ctxt (Val (Str ";")))
      )
    | _ -> None

  let call_html_makeargsetter (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; e2] ->
      Some (
        sprintf
          "the result of calling the <i><a href=\"#MakeArgSetter\">MakeArgSetter</a></i> abstract operation with arguments %s and %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_makeargsetterletparamauxfunction (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the String %s concatenated with the String %s"
          (to_html ctxt e1)
          (to_html ctxt (Val (Str "_arg")))
      )
    | _ -> None

  let call_html_makeargsetterletbodyauxfunction (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          (* &#60; is the HTML escape sequence for the symbol < *)
          (* &#62; is the HTML escape sequence for the symbol > *)
          "the String <b>\"&#60;name&#62; = &#60;param&#62;;\"</b> with <b>&#60;name&#62;</b> replaced by the value of %s and <b>&#60;param&#62;</b> replaced by the value of %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_createfunctionobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [NOpt (Oper.ListExpr, []); e1; e2; e3; _] ->
      Some (
        sprintf
          "the result of creating a function object as described in <a href=\"#sec-13.2\">13.2</a> using no <i>FormalParameterList</i>, %s for
          <i><a href=\"#sec-13\">FunctionBody</a></i>, %s as Scope, and %s for <i>Strict</i>"
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
      )
    | [NOpt (Oper.ListExpr, [Val (Str s)]); e1; e2; e3; _] ->
      Some (
        sprintf
          "the result of creating a function object as described in <a href=\"#sec-13.2\">13.2</a> using a <a href=\"#sec-8.8\">List</a> containing the single String
          %s as <i>FormalParameterList</i>, %s for <i><a href=\"#sec-13\">FunctionBody</a></i>, %s as <i>Scope</i>, and %s for <i>Strict</i>"
          (to_html ctxt (Var s))
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
      )
    | [e1; e2; e3; e4; _] ->
      Some (
        sprintf
          "a new Function object created as specified in <a href=\"#sec-13.2\">13.2</a> passing %s as the <i>FormalParameterList</i><sub>opt</sub>
          and %s as the <i>FunctionBody</i>. Pass in %s as the <i>Scope</i> parameter and %s as the <i>Strict</i> flag"
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
          (to_html ctxt e4)
      )
    | _ -> None

  let call_html_initialglobalexecutioncontext (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [_; Var "evalCode"] ->
      Some (
        "Initialise the execution context as if it was a global execution context using the <a href=\"#eval-code\">eval code</a>
        as <i>C</i> as described in <a href=\"#sec-10.4.1.1\">10.4.1.1</a>"
      )
    | _ ->
      Some (
        "Initialise the execution context using the <a href=\"#global-code\">global code</a> as described in <a href=\"#sec-10.4.1.1\">10.4.1.1</a>"
      )

  let call_html_enteringfunctioncode (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e_o; _; e_args] ->
      Some (
        sprintf
          "the result of establishing a new execution context for <a href=\"#function-code\">function code</a> using the value of %s's [[FormalParameters]]
          internal property, the passed arguments <a href=\"#sec-8.8\">List</a> %s, and the <b>this</b> value as described in <a href=\"#sec-10.4.3\">10.4.3</a>"
          (to_html ctxt e_o)
          (to_html ctxt e_args)
      )
    | _ -> None

  let call_html_isevalcode (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "%s is <a href=\"#eval-code\">eval code</a>"
          (to_html ctxt e)
      )
    | _ -> None

  let call_html_isstrictmodecode (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ Var "evalCode"; _ ] ->
      Some (
        "the <a href=\"#eval-code\">eval code</a> is <a href=\"#sec-10.1.1\">strict mode code</a>"
      )
    | [ e ]
    | [ e; _ ] ->
      Some (
        sprintf
          "%s is <a href=\"#sec-10.1.1\">strict mode code</a>"
          (to_html ctxt e)
      )
    | _ -> None

  let call_html_iscontainedinstrictcode (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "the syntactic production that is being evaluated is contained in <a href=\"#sec-10.1.1\">strict mode code</a>"
    )

  let call_html_isstrictfunctionobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "%s is a strict mode Function object"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_getfunctioncode (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "the <a href=\"#function-code\">function code</a>"
    )

  let call_html_isfunctioncode (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "%s is <a href=\"#function-code\">function code</a>"
          (to_html ctxt e)
      )
    | _ -> None


  let call_html_everyfieldisabsent (ctxt : ctxt_t) (e : t) (es: t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "every field in %s is absent"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_iszero (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "%s is +0"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_isminuszero (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        sprintf
          "%s is -0"
          (to_html TopLevel e)
      )
    | _ -> None

  let call_html_samenumber (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "%s is the same Number value as %s"
          (to_html TopLevel e1)
          (to_html TopLevel e2)
      )
    | _ -> None

  let call_html_sameobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "%s and %s refer to the same object"
          (to_html TopLevel e1)
          (to_html TopLevel e2)
      )
    | _ -> None

  let call_html_samesequence (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "%s and %s are exactly the same sequence of characters (same length and same characters in corresponding positions)"
          (to_html TopLevel e1)
          (to_html TopLevel e2)
      )
    | _ -> None

  let call_html_mathematicalvalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the mathematical value of %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_setthisbinding (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [_; e] ->
      Some (
        sprintf
          "Set the ThisBinding to %s"
          (to_html ctxt e)
      )
    | _ -> None

  let call_html_ignore_function_name (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e ] ->
      Some (
        to_html ctxt e
      )
    | _ -> None

  let call_html_isdirectcall (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "If there is no calling context or if the <a href=\"#eval-code\">eval code</a> is not being evaluated
      by a direct call (<a href=\"#sec-15.1.2.1.1\">15.1.2.1.1</a>) to the eval function"
    )

  let call_html_newecmascriptobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "a newly created native ECMAScript object"
    )

  let call_html_setallinternalmethodsofobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "Set all the internal methods of %s as specified in <a href=\"#sec-8.12\">8.12</a>"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_setallinternalmethodsexceptget (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "Set all the internal methods, except for [[Get]], of %s as described in <a href=\"#sec-8.12\">8.12</a>"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_listofidentifiersof (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "a <a href=\"#sec-8.8\">List</a> containing, in left to right textual order, the Strings corresponding to the identifiers of %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_numberofformalparameters (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the number of formal parameters specified in %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_isvalueanemptyfunctionbody (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "%s is an empty <i><a href=\"#sec-13\">FunctionBody</a></i>"
          (to_html ctxt e1)
      )
    | _ -> None


  let call_html_setinternalproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [obj; p; Val (Str s)] when is_special_val s ->
      Some (
        sprintf
          "Set the %s internal property of %s %s"
          (to_html ctxt p)
          (to_html ctxt obj)
          (get_special_val_html s)
      )
    | [obj; p; v] ->
      Some (
        sprintf
          "Set the %s internal property of %s to %s"
          (to_html ctxt p)
          (to_html ctxt obj)
          (to_html ctxt v)
      )
    | _ -> None

  let call_html_getinternalproperty (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [obj; p] ->
      Some (
        sprintf
          "the value of the %s internal property of %s"
          (to_html ctxt p)
          (to_html ctxt obj)
      )
    | _ -> None

  let call_html_getobjectprototype (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "the standard built-in Object prototype object (<a href=\"#sec-15.2.4\">15.2.4</a>)"
    )

  let call_html_normalemptycompletion (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "(normal, %s, empty)"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_getcompletiontype (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "%s.type"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_getcompletiontarget (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "%s.target"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_getcompletionvalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "%s.value"
          (to_html ctxt e1)
      )
    | _ -> None


  let call_html_exitexecutioncontext (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "Exit the execution context %s, restoring the previous execution context"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_isjavascriptobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the %s is a native ECMAScript object"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_ishostobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; _ ] ->
      Some (
        sprintf
          "the %s is a host object"
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_hostobjectreturn (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "a result in an implementation-dependent manner that may depend on the host object"
    )

  let call_html_hostobjectvalueof (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [e1; _] ->
      Some (
        sprintf
          "either %s or another value such as the host object originally passed to the constructor.
          The specific result that is returned is implementation-defined"
          (to_html ctxt e1)
      )
    |_ -> None

  let call_html_getownenumerableproperties (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "an internal list containing the names of each enumerable own property of %s"
          (to_html ctxt e1)
      )
    | _ -> None


  let call_html_createthrowtypeerrorfunctionobject (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "the [[ThrowTypeError]] function Object (<a href=\"#sec-13.2.3\">13.2.3</a>)"
    )

  let call_html_throwanyapplicableexceptions (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      "throw any exceptions specified in <a href=\"#sec-13.1\">13.1</a> that apply"
    )

  let call_html_applyingtheadditionoperation (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "the result of applying the addition operation to %s and %s. See the Note below <a href=\"#sec-11.6.3\">11.6.3</a>"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_applyingthesubtractionoperation (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "the result of applying the subtraction operation to %s and %s. See the Note below <a href=\"#sec-11.6.3\">11.6.3</a>"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_applybitwiseoperator (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ _; e1; e2 ] ->
      Some (
        sprintf
          "the result of applying the bitwise operator @ to %s and %s. The result is a signed 32 bit integer"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None

  let call_html_applyingmultiplicativeoperator (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1; e2 ] ->
      Some (
        sprintf
          "the result of applying the specified operation (*, /, or %%) to %s and %s. See the Notes
          below <a href=\"#sec-11.5.1\">11.5.1</a>, <a href=\"#sec-11.5.2\">11.5.2</a>, <a href=\"#sec-11.5.3\">11.5.3</a>"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    |_ -> None

  let call_html_applyoperator (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ _; e1; e2] ->
      Some (
        sprintf
          "the result of applying operator @ to %s and %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | _ -> None



  let call_html_maskoutbits (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ e1 ] ->
      Some (
        sprintf
          "the result of masking out all but the least significant 5 bits of %s, that is, compute %s & 0x1F"
          (to_html ctxt e1)
          (to_html ctxt e1)
      )
    | _ -> None

  let call_html_newpropertyreference (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match es with
    | [ baseValue; referencedName; strictModeFlag ] ->
      Some (
        sprintf
          "a value of type <a href=\"#sec-8.7\">Reference</a> whose base value is %s and whose referenced name
            is %s, and whose strict mode flag is %s"
          (to_html ctxt baseValue)
          (to_html ctxt referencedName)
          (to_html ctxt strictModeFlag)
      )
    | _ -> None

  let call_html_exception (exception_name : string) (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    Some (
      sprintf
        "a <b>%s</b> exception"
        exception_name
    )


  let oper_html_binary (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    let op_html =
      match op with
      | ">" -> "is greater than"
      | "<" -> "is less than"
      | "+" -> "plus"
      | "-" -> "minus"
      | _   -> op in
    Some (
      sprintf
        "%s %s %s"
        (to_html ctxt e1)
        op_html
        (to_html ctxt e2)
    )

  let oper_html_lnth (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    match e2 with
    | BinOpt (Oper.Minus, x, Val (Int 1)) ->
      Some (
        sprintf
          "the value of the %s'th element of %s"
          (to_html ctxt x)
          (to_html ctxt e1)
      )
    | Var _ ->
      Some (
        sprintf
          "the element of %s at 0-origined list position %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
      )
    | Val Int 0 ->
      Some (
        sprintf
          "the first element of %s"
          (to_html ctxt e1)
      )
    | Val Int 1 ->
      Some (
        sprintf
          "the second element of %s"
          (to_html ctxt e1)
      )
    | _ -> None

  let oper_html_ladd (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    Some (
      sprintf
        "Add %s as an element of the list %s"
        (to_html ctxt e2)
        (to_html ctxt e1)
    )

  let oper_html_inobj (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    match e1, e2, ctxt with
    | _, Lookup (e2', Val (Str "JSProperties")), Negative ->
      Some (
        sprintf
          "%s doesn’t have an own property with name %s"
          (to_html TopLevel e2')
          (to_html TopLevel e1)
      )
    | _, Lookup (e2', Val (Str "JSProperties")), _ ->
      Some (
        sprintf "%s has a property with name %s"
          (to_html ctxt e2')
          (to_html ctxt e1)
      )
    | _, Var "envRec", Negative ->
      Some (
        sprintf
          "%s does not have a binding for the name that is the value of %s"
          (to_html TopLevel e2)
          (to_html TopLevel e1)
      )
    | _, Var "envRec", AssertNegative ->
      Some (
        sprintf
          "%s does not already have a binding for %s"
          (to_html TopLevel e2)
          (to_html TopLevel e1)
      )
    | _, Var "envRec", Assert ->
      Some (
        sprintf
          "%s has a binding for %s"
          (to_html TopLevel e2)
          (to_html TopLevel e1)
      )
    | _, Var "envRec", _ ->
      Some (
        sprintf
          "%s has a binding for the name that is the value of %s"
          (to_html ctxt e2)
          (to_html ctxt e1)
      )
    | Val (Str x), Var _, Negative when is_internal_method x ->
      Some (
        sprintf "%s has no [[%s]] internal method"
          (to_html TopLevel e2)
          x
      )
    | Val (Str x), Var _, _ when is_internal_method x ->
      Some (
        sprintf "%s has a [[%s]] internal method"
          (to_html ctxt e2)
          x
      )
    | _, Var _, Negative ->
      Some (
        sprintf
          "%s.%s is absent"
          (to_html TopLevel e2)
          (to_html TopLevel e1)
      )
    | _, Var _, _ ->
      Some (
        sprintf
          "%s.%s is present"
          (to_html ctxt e2)
          (to_html ctxt e1)
      )
    | _ -> None

  let oper_html_inlist (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    let ctxt_str =
      match ctxt with
      | Negative -> "is not"
      | _        -> "is" in
    Some (
      sprintf
        "%s %s an element of %s"
        (to_html BinaryExpr e1)
        ctxt_str
        (to_html BinaryExpr e2)
    )

  let oper_html_equal (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    let ctxt_str =
      match ctxt with
      | Negative -> "is not"
      | _ -> "is" in
    match e2, ctxt with
    | GVar "global", _ ->
      Some (
        sprintf
          "%s is the <a href=\"#sec-10.2\">environment record</a> component of the global environment"
          (to_html BinaryExpr e1)
      )
    | Val Null, Negative ->
      Some (
        sprintf
          "%s is supplied"
          (to_html BinaryExpr e1)
      )
    | Val Null, _ ->
      Some (
        sprintf
          "%s is not supplied"
          (to_html BinaryExpr e1)
      )
    | _ ->
      Some (
        sprintf
          "%s %s %s"
          (to_html BinaryExpr e1)
          ctxt_str
          (to_html BinaryExpr e2)
      )

  let oper_html_logical (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    let op_html =
      match op with
      | "&&" | "&&&" -> "and"
      | "||" | "|||" -> "or"
      | _            -> op in
    Some (
      sprintf
        "%s %s %s"
        (to_html BinaryExpr e1)
        op_html
        (to_html BinaryExpr e2)
    )

  let oper_html_left_shift (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    Some (
      sprintf
        "the result of left shifting %s by %s bits. The result is a signed 32-bit integer"
        (to_html ctxt e1)
        (to_html ctxt e2)
    )

  let oper_html_right_shift (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    Some (
      sprintf
        "the result of performing a sign-extending right shift of %s by %s bits. The most significant bit is propagated.
        The result is a signed 32-bit integer"
        (to_html ctxt e1)
        (to_html ctxt e2)
    )

  let oper_html_right_shift_logical (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) : string option =
    Some (
      sprintf
        "the result of performing a zero-filling right shift of %s by %s bits. Vacated bits are filled with zero.
        The result is an unsigned 32-bit integer"
        (to_html ctxt e1)
        (to_html ctxt e2)
    )

  let oper_html_unary_neg (ctxt : ctxt_t) (op : string) (e : t) (_) : string option =
    match ctxt, op with
    | Assert, "!"    -> Some (to_html AssertNegative e)
    | _, "!"| _, "-" -> Some (to_html Negative e)
    | _ -> None

  let oper_html_unary_len (ctxt : ctxt_t) (op : string) (e : t) (_) : string option =
    match op with
    | "l_len" ->
      Some (
        sprintf
          "the number of elements in %s"
          (to_html ctxt e)
      )
    | _ -> None

  let oper_html_sconcat (ctxt : ctxt_t) (op : string) (e : t) (_) : string option =
    match e with
    | NOpt (Oper.ListExpr, [e1; e2; e3]) ->
      Some (
        sprintf
          "the String value that is the result of concatenating the three Strings %s, %s, and %s"
          (to_html ctxt e1)
          (to_html ctxt e2)
          (to_html ctxt e3)
      )
    | NOpt (Oper.ListExpr, es) ->
      Some (
        sprintf
          "the String that is the result of concatenating %s"
          (
            match es with
            | []      -> invalid_arg "Exception in oper_html_sconcat: unexpected empty list."
            | e :: [] -> (to_html ctxt e)
            | es'     ->
              let rev_es = List.rev es' in
              let last_e = List.hd rev_es in
              let rest_es = List.tl rev_es in
              sprintf
                "%s, and %s"
                (String.concat ", " (List.rev_map (to_html ctxt) rest_es))
                (to_html ctxt last_e)
          )
      )
    | _ -> None

  let oper_html_bitwisenot (ctxt : ctxt_t) (op : string) (e : t) (_) : string option =
    Some (
      sprintf
        "the result of applying bitwise complement to %s. The result is a signed 32-bit integer"
        (to_html ctxt e)
    )

  let oper_hmtl_print_ignore (ctxt : ctxt_t) (op : string) (e : t) (_) : string option =
    Some (
      to_html ctxt e
    )

  let () =
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Plus) oper_html_binary;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Minus) oper_html_binary;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Gt) oper_html_binary;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Lt) oper_html_binary;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.InObj) oper_html_inobj;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.InList) oper_html_inlist;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Lnth) oper_html_lnth;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Ladd) oper_html_ladd;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Equal) oper_html_equal;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Log_And) oper_html_logical;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.Log_Or) oper_html_logical;
    Hashtbl.add binoper_hashtable_html (EOper.str_of_binopt_single EOper.SCLogAnd) oper_html_logical;
    Hashtbl.add binoper_hashtable_html (EOper.str_of_binopt_single EOper.SCLogOr) oper_html_logical;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.ShiftLeft) oper_html_left_shift;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.ShiftRight) oper_html_right_shift;
    Hashtbl.add binoper_hashtable_html (Oper.str_of_binopt_single Oper.ShiftRightLogical) oper_html_right_shift_logical;
    Hashtbl.add unoper_hashtable_html (Oper.str_of_unopt Oper.IntToFloat) oper_hmtl_print_ignore;
    Hashtbl.add unoper_hashtable_html (Oper.str_of_unopt Oper.Neg) oper_html_unary_neg;
    Hashtbl.add unoper_hashtable_html (Oper.str_of_unopt Oper.Not) oper_html_unary_neg;
    Hashtbl.add unoper_hashtable_html (Oper.str_of_unopt Oper.ListLen) oper_html_unary_len;
    Hashtbl.add unoper_hashtable_html (Oper.str_of_unopt Oper.Sconcat) oper_html_sconcat;
    Hashtbl.add unoper_hashtable_html (Oper.str_of_unopt Oper.BitwiseNot) oper_html_bitwisenot;
    Hashtbl.add lookup_hashtable_html "\"JSProperties\"" lookup_html_ownproperty;
    Hashtbl.add lookup_hashtable_html "\"Value\"" (lookup_html_attribute "Value");
    Hashtbl.add lookup_hashtable_html "\"Writable\"" (lookup_html_attribute "Writable");
    Hashtbl.add lookup_hashtable_html "\"Set\"" (lookup_html_attribute "Set");
    Hashtbl.add lookup_hashtable_html "\"Get\"" (lookup_html_attribute "Get");
    Hashtbl.add lookup_hashtable_html "\"Put\"" (lookup_html_attribute "Put");
    Hashtbl.add lookup_hashtable_html "\"Enumerable\"" (lookup_html_attribute "Enumerable");
    Hashtbl.add lookup_hashtable_html "\"Configurable\"" (lookup_html_attribute "Configurable");
    Hashtbl.add lookup_hashtable_html "\"Prototype\"" (lookup_html_internal_property "Prototype");
    Hashtbl.add lookup_hashtable_html "\"Extensible\"" (lookup_html_internal_property "Extensible");
    Hashtbl.add lookup_hashtable_html "\"FormalParameters\"" (lookup_html_internal_property "FormalParameters");
    Hashtbl.add lookup_hashtable_html "\"ParameterMap\"" (lookup_html_internal_property "ParameterMap");
    Hashtbl.add lookup_hashtable_html "\"GetOwnProperty\"" (lookup_html_internal_property "GetOwnProperty");
    Hashtbl.add lookup_hashtable_html "\"DefineOwnProperty\"" (lookup_html_internal_property "DefineOwnProperty");
    Hashtbl.add lookup_hashtable_html "\"Scope\"" (lookup_html_internal_property "Scope");
    Hashtbl.add lookup_hashtable_html "\"Code\"" (lookup_html_internal_property "Code");
    Hashtbl.add call_hashtable_html "\"Call\"" call_html_call;
    Hashtbl.add call_hashtable_html "\"GetOwnProperty\"" (call_html_internal_method "GetOwnProperty");
    Hashtbl.add call_hashtable_html "\"GetProperty\"" (call_html_internal_method "GetProperty");
    Hashtbl.add call_hashtable_html "\"Get\"" (call_html_internal_method "Get");
    Hashtbl.add call_hashtable_html "<i>get</i>" (call_html_internal_method ~print_brackets:false "get");
    Hashtbl.add call_hashtable_html "\"CanPut\"" (call_html_internal_method "CanPut");
    Hashtbl.add call_hashtable_html "\"Put\"" (call_html_internal_method "Put");
    Hashtbl.add call_hashtable_html "Put" (call_html_internal_method "Put");
    Hashtbl.add call_hashtable_html "<i>put</i>" (call_html_internal_method ~print_brackets:false "put");
    Hashtbl.add call_hashtable_html "\"HasProperty\"" (call_html_internal_method "HasProperty");
    Hashtbl.add call_hashtable_html "\"Delete\"" (call_html_internal_method "Delete");
    Hashtbl.add call_hashtable_html "\"DefaultValue\"" (call_html_internal_method "DefaultValue");
    Hashtbl.add call_hashtable_html "\"DefineOwnProperty\"" (call_html_internal_method "DefineOwnProperty");
    Hashtbl.add call_hashtable_html "\"HasInstance\"" (call_html_internal_method "HasInstance");
    Hashtbl.add call_hashtable_html "\"Construct\"" (call_html_internal_method "Construct");
    Hashtbl.add call_hashtable_html "Get" (call_html_default_method "Get");
    Hashtbl.add call_hashtable_html "GetOwnProperty" (call_html_default_method "GetOwnProperty");
    Hashtbl.add call_hashtable_html "DefineOwnProperty" (call_html_default_method "DefineOwnProperty");
    Hashtbl.add call_hashtable_html "Delete" (call_html_default_method "Delete");
    Hashtbl.add call_hashtable_html "DefaultValue" (call_html_custom_method "DefaultValue");
    Hashtbl.add call_hashtable_html "ObjectConstructor" (call_html_constructor "Object");
    Hashtbl.add call_hashtable_html "BooleanConstructor" (call_html_constructor "Boolean");
    Hashtbl.add call_hashtable_html "NumberConstructor" (call_html_constructor "Number");
    Hashtbl.add call_hashtable_html "StringConstructor" (call_html_constructor "String");
    Hashtbl.add call_hashtable_html "ArrayConstructor" (call_html_constructor "Array");
    Hashtbl.add call_hashtable_html "ObjectDefineProperties" call_html_objectdefineproperties_builtin_method;
    Hashtbl.add call_hashtable_html "PropertyDescriptor" call_html_property_descriptor;
    Hashtbl.add call_hashtable_html "FromPropertyDescriptor" call_html_frompropertydescriptor;
    Hashtbl.add call_hashtable_html "ToPropertyDescriptor" call_html_topropertydescriptor;
    Hashtbl.add call_hashtable_html "getJSProperty" call_html_ownproperty;
    Hashtbl.add call_hashtable_html "Type" call_html_type;
    Hashtbl.add call_hashtable_html "GetBase" (call_html_reference_abstract_operations "GetBase");
    Hashtbl.add call_hashtable_html "GetReferencedName" (call_html_reference_abstract_operations "GetReferencedName");
    Hashtbl.add call_hashtable_html "IsStrictReference" (call_html_reference_abstract_operations "IsStrictReference");
    Hashtbl.add call_hashtable_html "HasPrimitiveBase" (call_html_reference_abstract_operations "HasPrimitiveBase");
    Hashtbl.add call_hashtable_html "IsPropertyReference" (call_html_reference_abstract_operations "IsPropertyReference");
    Hashtbl.add call_hashtable_html "IsUnresolvableReference" (call_html_reference_abstract_operations "IsUnresolvableReference");
    Hashtbl.add call_hashtable_html "GetValue" (call_html_reference_abstract_operations "GetValue");
    Hashtbl.add call_hashtable_html "PutValue" (call_html_reference_abstract_operations "PutValue");
    Hashtbl.add call_hashtable_html "ToPrimitive" (call_html_conversion_abstract_operations "ToPrimitive");
    Hashtbl.add call_hashtable_html "ToBoolean" (call_html_conversion_abstract_operations "ToBoolean");
    Hashtbl.add call_hashtable_html "ToNumber" (call_html_conversion_abstract_operations "ToNumber");
    Hashtbl.add call_hashtable_html "ToInteger" (call_html_conversion_abstract_operations "ToInteger");
    Hashtbl.add call_hashtable_html "ToInt32" (call_html_conversion_abstract_operations "ToInt32");
    Hashtbl.add call_hashtable_html "ToUint32" (call_html_conversion_abstract_operations "ToUint32");
    Hashtbl.add call_hashtable_html "ToUint16" (call_html_conversion_abstract_operations "ToUint16");
    Hashtbl.add call_hashtable_html "ToString" (call_html_conversion_abstract_operations "ToString");
    Hashtbl.add call_hashtable_html "ToObject" (call_html_conversion_abstract_operations "ToObject");
    Hashtbl.add call_hashtable_html "CheckObjectCoercible" (call_html_conversion_abstract_operations "CheckObjectCoercible");
    Hashtbl.add call_hashtable_html "IsPrimitiveValue" call_html_isprimitivevalue;
    Hashtbl.add call_hashtable_html "IsCallable" call_html_iscallable;
    Hashtbl.add call_hashtable_html "IsDataPropertyDescriptor" call_html_dataproperty;
    Hashtbl.add call_hashtable_html "IsAccessorPropertyDescriptor" call_html_accessorproperty;
    Hashtbl.add call_hashtable_html "IsGenericPropertyDescriptor" call_html_genericproperty;
    Hashtbl.add call_hashtable_html "SameValue" call_html_samevalue;
    Hashtbl.add call_hashtable_html "HasBinding" call_html_hasbinding_method;
    Hashtbl.add call_hashtable_html "DeleteBinding" call_html_deletebinding_method;
    Hashtbl.add call_hashtable_html "GetBindingValue" call_html_getbindingvalue_method;
    Hashtbl.add call_hashtable_html "CreateMutableBinding" call_html_createmutablebinding_method;
    Hashtbl.add call_hashtable_html "CreateImmutableBinding" call_html_createimmutablebinding_method;
    Hashtbl.add call_hashtable_html "InitializeImmutableBinding" call_html_initializeimmutablebinding_method;
    Hashtbl.add call_hashtable_html "SetMutableBinding" call_html_setmutablebinding;
    Hashtbl.add call_hashtable_html "ImplicitThisValue" call_html_implicitthisvalue;
    Hashtbl.add call_hashtable_html "CreateArgumentsObject" call_html_createargumentsobject;
    Hashtbl.add call_hashtable_html "NewDeclarativeEnvironment" call_html_newdeclarativeenvironment;
    Hashtbl.add call_hashtable_html "InitialGlobalExecutionContext" call_html_initialglobalexecutioncontext;
    Hashtbl.add call_hashtable_html "EnteringFunctionCode" call_html_enteringfunctioncode;
    Hashtbl.add call_hashtable_html "DeclarationBindingInstantiation" call_html_declarationbindinginstantiation;
    Hashtbl.add call_hashtable_html "MakeArgGetter" call_html_makearggetter;
    Hashtbl.add call_hashtable_html "makeArgGetterLetBodyAuxFunction" call_html_makearggetterletbodyauxfunction;
    Hashtbl.add call_hashtable_html "MakeArgSetter" call_html_makeargsetter;
    Hashtbl.add call_hashtable_html "makeArgSetterLetBodyAuxFunction" call_html_makeargsetterletbodyauxfunction;
    Hashtbl.add call_hashtable_html "makeArgSetterLetParamAuxFunction" call_html_makeargsetterletparamauxfunction;
    Hashtbl.add call_hashtable_html "CreateFunctionObject" call_html_createfunctionobject;
    Hashtbl.add call_hashtable_html "JS_Interpreter_Expr" call_html_evaluating;
    Hashtbl.add call_hashtable_html "JS_Interpreter_Arguments" call_html_evaluatingarguments;
    Hashtbl.add call_hashtable_html "JS_Interpreter_FunctionDeclaration" call_html_functiondeclaration;
    Hashtbl.add call_hashtable_html "JS_Interpreter_FunctionExpression" call_html_evaluating;
    Hashtbl.add call_hashtable_html "JS_Interpreter_FunctionBody" call_html_functionbody;
    Hashtbl.add call_hashtable_html "AbstractRelationalComparison" call_html_abstractrelationalcomparison;
    Hashtbl.add call_hashtable_html "AbstractEqualityComparison" call_html_abstractequalitycomparison;
    Hashtbl.add call_hashtable_html "StrictEqualityComparison" call_html_strictequalitycomparison;
    Hashtbl.add call_hashtable_html "convertDataPropertyDescToAccessorPropertyDesc" call_html_dataproperty_to_accessorproperty;
    Hashtbl.add call_hashtable_html "convertAccessorPropertyDescToDataPropertyDesc" call_html_accessorproperty_to_dataproperty;
    Hashtbl.add call_hashtable_html "createOwnAccessorProperty" call_html_createownaccessorproperty;
    Hashtbl.add call_hashtable_html "createOwnDataProperty" call_html_createowndataproperty;
    Hashtbl.add call_hashtable_html "setCorrespondinglyNamedAttributes" call_html_setcorrespondinglynamedattributes;
    Hashtbl.add call_hashtable_html "everyFieldInDescAlsoOccursInCurrent" call_html_everyfieldindescalsooccursincurrent;
    Hashtbl.add call_hashtable_html "createMutableBinding" call_html_createmutablebinding;
    Hashtbl.add call_hashtable_html "setBindingDeletable" call_html_setbindingdeletable;
    Hashtbl.add call_hashtable_html "isMutableBinding" call_html_ismutablebinding;
    Hashtbl.add call_hashtable_html "setBindingValue" call_html_setbindingvalue;
    Hashtbl.add call_hashtable_html "isUninitialisedBinding" call_html_isuninitialisedbinding;
    Hashtbl.add call_hashtable_html "getBindingValue" call_html_getbindingvalue;
    Hashtbl.add call_hashtable_html "isBindingCannotBeDeleted" call_html_isbindingcannotbedeleted;
    Hashtbl.add call_hashtable_html "hasUninitialisedImmutableBinding" call_html_hasuninitialisedimmutablebinding;
    Hashtbl.add call_hashtable_html "createImmutableBinding" call_html_createimmutablebinding;
    Hashtbl.add call_hashtable_html "setBindingInitialised" call_html_setbindinginitialised;
    Hashtbl.add call_hashtable_html "getBindingObject" call_html_getbindingobject;
    Hashtbl.add call_hashtable_html "getProvideThis" call_html_getprovidethis;
    Hashtbl.add call_hashtable_html "getThisBinding" call_html_getthisbinding;
    Hashtbl.add call_hashtable_html "newValueReference" call_html_newvaluereference;
    Hashtbl.add call_hashtable_html "getGlobalEnvironment" call_html_getglobalenvironment;
    Hashtbl.add call_hashtable_html "getOuterEnvironmentReference" call_html_getouterenvironmentreference;
    Hashtbl.add call_hashtable_html "setOuterLexicalEnvironmentReference" call_html_setouterlexicalenvironmentreference;
    Hashtbl.add call_hashtable_html "newLexicalEnvironment" call_html_newlexicalenvironment;
    Hashtbl.add call_hashtable_html "setLexicalEnvironment" call_html_setlexicalenvironment;
    Hashtbl.add call_hashtable_html "getLexicalEnvironment" call_html_getlexicalenvironment;
    Hashtbl.add call_hashtable_html "setVariableEnvironment" call_html_setvariableenvironment;
    Hashtbl.add call_hashtable_html "getVariableEnvironment" call_html_getvariableenvironment;
    Hashtbl.add call_hashtable_html "newDeclarativeEnvironmentRecord" call_html_newdeclarativeenvironmentrecord;
    Hashtbl.add call_hashtable_html "getEnvironmentRecord" call_html_getenvironmentrecord;
    Hashtbl.add call_hashtable_html "setEnvironmentRecord" call_html_setenvironmentrecord;
    Hashtbl.add call_hashtable_html "newObjectEnvironmentRecord" call_html_newobjectenvironmentrecord;
    Hashtbl.add call_hashtable_html "getEnvRecOfRunningExecCtx" call_html_getenvrecofrunningexecctx;
    Hashtbl.add call_hashtable_html "isEvalCode" call_html_isevalcode;
    Hashtbl.add call_hashtable_html "isStrictModeCode" call_html_isstrictmodecode;
    Hashtbl.add call_hashtable_html "isContainedInStrictCode" call_html_iscontainedinstrictcode;
    Hashtbl.add call_hashtable_html "isBuiltInFunctionBodyStrictModeCode" call_html_isstrictmodecode;
    Hashtbl.add call_hashtable_html "isStrictFunctionObject" call_html_isstrictfunctionobject;
    Hashtbl.add call_hashtable_html "isFunctionCode" call_html_isfunctioncode;
    Hashtbl.add call_hashtable_html "getFunctionCode" call_html_getfunctioncode;
    Hashtbl.add call_hashtable_html "isZero" call_html_iszero;
    Hashtbl.add call_hashtable_html "isMinusZero" call_html_isminuszero;
    Hashtbl.add call_hashtable_html "sameNumber" call_html_samenumber;
    Hashtbl.add call_hashtable_html "sameObject" call_html_sameobject;
    Hashtbl.add call_hashtable_html "sameSequenceOfCharacters" call_html_samesequence;
    Hashtbl.add call_hashtable_html "mathematicalValue" call_html_mathematicalvalue;
    Hashtbl.add call_hashtable_html "setThisBinding" call_html_setthisbinding;
    Hashtbl.add call_hashtable_html "EveryFieldIsAbsent" call_html_everyfieldisabsent;
    Hashtbl.add call_hashtable_html "getIdentifierFunctionDeclaration" call_html_getidentifierfunctiondeclaration;
    Hashtbl.add call_hashtable_html "getIdentifierVariableDeclaration" call_html_getidentifiervariabledeclaration;
    Hashtbl.add call_hashtable_html "getStringValue" call_html_getstringvalue;
    Hashtbl.add call_hashtable_html "getFunctionDeclarationsInCode" call_html_ignore_function_name;
    Hashtbl.add call_hashtable_html "getVariableDeclarationsInCode" call_html_ignore_function_name;
    Hashtbl.add call_hashtable_html "getOwnProperties" call_html_ignore_function_name;
    Hashtbl.add call_hashtable_html "isDirectCall" call_html_isdirectcall;
    Hashtbl.add call_hashtable_html "NewECMAScriptObject" call_html_newecmascriptobject;
    Hashtbl.add call_hashtable_html "setAllInternalMethodsOfObject" call_html_setallinternalmethodsofobject;
    Hashtbl.add call_hashtable_html "setAllInternalMethodsExceptGet" call_html_setallinternalmethodsexceptget;
    Hashtbl.add call_hashtable_html "listOfIdentifiersOf" call_html_listofidentifiersof;
    Hashtbl.add call_hashtable_html "numberOfFormalParameters" call_html_numberofformalparameters;
    Hashtbl.add call_hashtable_html "isValueAnEmptyFunctionBody" call_html_isvalueanemptyfunctionbody;
    Hashtbl.add call_hashtable_html "setInternalProperty" call_html_setinternalproperty;
    Hashtbl.add call_hashtable_html "getInternalProperty" call_html_getinternalproperty;
    Hashtbl.add call_hashtable_html "getObjectPrototype" call_html_getobjectprototype;
    Hashtbl.add call_hashtable_html "normalEmptyCompletion" call_html_normalemptycompletion;
    Hashtbl.add call_hashtable_html "exitExecutionContext" call_html_exitexecutioncontext;
    Hashtbl.add call_hashtable_html "getCompletionType" call_html_getcompletiontype;
    Hashtbl.add call_hashtable_html "getCompletionTarget" call_html_getcompletiontarget;
    Hashtbl.add call_hashtable_html "getCompletionValue" call_html_getcompletionvalue;
    Hashtbl.add call_hashtable_html "exitExecutionContext" call_html_exitexecutioncontext;
    Hashtbl.add call_hashtable_html "isJavaScriptObject" call_html_isjavascriptobject;
    Hashtbl.add call_hashtable_html "isHostObject" call_html_ishostobject;
    Hashtbl.add call_hashtable_html "hostObjectReturn" call_html_hostobjectreturn;
    Hashtbl.add call_hashtable_html "hostObjectValueOf" call_html_hostobjectvalueof;
    Hashtbl.add call_hashtable_html "getOwnEnumerableProperties" call_html_getownenumerableproperties;
    Hashtbl.add call_hashtable_html "createThrowTypeErrorFunctionObject" call_html_createthrowtypeerrorfunctionobject;
    Hashtbl.add call_hashtable_html "throwAnyApplicableExceptions" call_html_throwanyapplicableexceptions;
    Hashtbl.add call_hashtable_html "applyingTheAdditionOperation" call_html_applyingtheadditionoperation;
    Hashtbl.add call_hashtable_html "applyingTheSubtractionOperation" call_html_applyingthesubtractionoperation;
    Hashtbl.add call_hashtable_html "applyBitwiseOperator" call_html_applybitwiseoperator;
    Hashtbl.add call_hashtable_html "applyingTheMultiplicationOperator" call_html_applyingmultiplicativeoperator;
    Hashtbl.add call_hashtable_html "applyingTheDivisionOperator" call_html_applyingmultiplicativeoperator;
    Hashtbl.add call_hashtable_html "applyingTheRemainderOperator" call_html_applyingmultiplicativeoperator;
    Hashtbl.add call_hashtable_html "applyOperator" call_html_applyoperator;
    Hashtbl.add call_hashtable_html "maskOutBits" call_html_maskoutbits;
    Hashtbl.add call_hashtable_html "newPropertyReference" call_html_newpropertyreference;
    Hashtbl.add call_hashtable_html "NewPropertyDescriptor" (fun _ _ _ -> Some "a newly created <a href=\"#sec-8.10\">Property Descriptor</a> with no fields");
    Hashtbl.add call_hashtable_html "SyntaxErrorConstructorInternal" (call_html_exception "SyntaxError");
    Hashtbl.add call_hashtable_html "TypeErrorConstructorInternal" (call_html_exception "TypeError");
    Hashtbl.add call_hashtable_html "ReferenceErrorConstructorInternal" (call_html_exception "ReferenceError");

end

module E_Stmt = struct
  include E_Stmt

  open HTMLHashtables.E_Stmt

  type ctxt_t =
    | CNone
    | Prepend       of string
    | Append        of string
    | Replace       of string
    | SameParagraph
    | Table
    | MatchWith

  let is_split_if (meta : metadata_t list) : bool =
    try
      let m = List.hd meta in
      match m.where with
      | "split-if" -> true
      | _          -> false
    with Failure _ -> false

  let parse_if_split_meta (meta : metadata_t list) : string =
    try
      let m = List.hd meta in
      match m.where with
      | "split-if" -> m.html
      | _          -> ""
    with Failure _ -> ""

  let parse_if_meta (meta : metadata_t list) (ctxt : ctxt_t) : string * ctxt_t =
    List.fold_left (
      fun acc m ->
        match m.where with
        | "before-same"  -> (fst acc), Prepend m.html
        | "after-same"   -> m.html, (snd acc)
        | "after-next"   -> (fst acc), Append m.html
        | "replace-with" -> (fst acc), Replace m.html
        | _              -> acc
    ) ("", ctxt) meta

  let has_after_same_meta (meta : metadata_t list) : bool =
    List.exists (fun m -> m.where = "after-same") meta

  let is_basic_if (s : t) : bool =
    is_basic s ||
    match s with
    | If (e, s1, None, _, _) -> is_basic s1
    | _                      -> false

  let is_special_expr (e : E_Expr.t) : bool =
    match e with
    | Call (Val (Str "InitialGlobalExecutionContext"), _, _)
    | BinOpt (Oper.Ladd, _, _) -> true
    | _                        -> false

  let is_basic_and_not_call (s : t) : bool =
    is_basic_if s &&
    match s with
    | ExprStmt e ->
      (match e with
       | Call _ -> false
       | _      -> true
      )
    | _ -> true

  let is_special_assign_expr (s : t) : bool =
    match s with
    | Assign (_, e) when is_special_expr e -> true
    | _                                    -> false

  let is_let_stmt (s : t) : bool =
    match s with
    | Assign (x, e) -> true
    | _             -> false

  let swappable (s : t) : bool =
    match s with
    | MacroApply (f, _) -> Hashtbl.mem swappable_hashtable_html f
    | _ -> false


  let rec to_html ?(ctxt=CNone) (stmt : t) : string * ctxt_t =
    let expr_to_html = E_Expr.to_html TopLevel in
    let ctxt', prepend_str, append_str, replace_str =
      match ctxt with
      | CNone
      | SameParagraph
      | Table
      | MatchWith -> ctxt, "", "", ""
      | Prepend s -> CNone, s, "", ""
      | Append s  -> CNone, "", s, ""
      | Replace s -> (CNone, "", "", s) in
    match stmt with
    | Skip
    | Print _
    | GlobAssign _
    | Fail _ -> "", ctxt'

    | MatchWith (_, e_pats) ->
      invalid_arg "MatchWith"
    (* String.concat "" (List.map (
        fun ((e_pat : E_Pat.t), (s : E_Stmt.t)) ->
          (match e_pat with
           | DefaultPat
           | ObjPat (_, None) -> ""
           | ObjPat (_, Some meta) ->
             let params_meta = E_Pat_Metadata.get_meta_params meta in
             HTMLHashtables.E_Expr.init_var_bindings params_meta;
             let prod_number = E_Pat_Metadata.get_production_number meta in
             let prod_name = E_Pat_Metadata.get_production_name meta in
             let prod_text = E_Pat_Metadata.get_production_text meta in
             let prod_pre = E_Pat_Metadata.get_pre meta in
             let prod_post = E_Pat_Metadata.get_post meta in
             let s_html, _ = to_html ~ctxt:ctxt' s in
             match prod_name with
             | None ->
               sprintf
                 "%s
                  <p>The production <span class=\"prod\">%s</span> is evaluated as follows:</p>
                  <ol class=\"proc\">%s</ol>
                  %s"
                 (if prod_pre <> "" then sprintf "<p>%s</p>" prod_pre else "")
                 prod_text
                 s_html
                 (if prod_post <> "" then sprintf "<p>%s</p>" prod_post else "")
             | Some s ->
               sprintf
                 "<section id=\"sec-%s\">
                    <h1><span class=\"secnum\">
                      <a href=\"#sec-%s\" title=\"link to this section\">%s</a>
                    </span>%s</h1>
                    %s
                    <p>The production <span class=\"prod\">%s</span> is evaluated as follows:</p>
                    <ol class=\"proc\">%s</ol>
                    %s
                  </section>"
                 prod_number
                 prod_number
                 prod_number
                 s
                 (if prod_pre <> "" then sprintf "<p>%s</p>" prod_pre else "")
                 prod_text
                 s_html
                 (if prod_post <> "" then sprintf "<p>%s</p>" prod_post else ""))
       ) e_pats), MatchWith *)

    | Assert e ->
      let e_html = (E_Expr.to_html Assert e) in
      (sprintf
         "<li>Assert: %s.</li>"
         e_html
      ), ctxt'

    | Wrapper (meta, s) ->
      let m = List.hd meta in
      (match m.where with
       | "before-same"  -> to_html ~ctxt:(Prepend m.html) s
       | "after-same"   -> to_html ~ctxt:(Append m.html) s
       | _ -> "", ctxt')

    | MacroApply (x, es) ->
      let f_x = Hashtbl.find_opt macro_hashtable_html x in
      let res_x = Option.map_default (fun f -> f) None f_x in
      sprintf
        "%s"
        (match res_x with
         | Some s -> s
         | None   -> x), ctxt'

    | ExprStmt e ->
      sprintf
        "<li>%s%s. %s</li>"
        prepend_str
        (E_Expr.to_html ExprStmt e)
        append_str, ctxt'

    | Assign (x, e) when is_special_expr e ->
      sprintf
        "<li>%s.</li>"
        (expr_to_html e), ctxt'

    | Assign (x, e) ->
      let contents =
        sprintf
          "<i>%s</i> be %s%s"
          x
          (E_Expr.to_html Let e)
          append_str in
      (match ctxt' with
       | SameParagraph ->
         sprintf
           "%slet %s"
           prepend_str
           contents
       | _ ->
         sprintf
           "<li>%s%s %s.</li>"
           prepend_str
           (if prepend_str = "" then "Let" else "let")
           contents), ctxt'

    | Return e when ctxt' = Table ->
      let html =
        match e with
        | Var _    -> "The result equals the input argument (no conversion)."
        | Val Null -> "Return"
        | _        -> E_Expr.to_html Table e in
      html, CNone

    | Return e ->
      (match ctxt, e with
       | SameParagraph, Val Null ->
         sprintf
           "%sreturn%s."
           prepend_str
           append_str, CNone
       | SameParagraph, _ ->
         sprintf
           "%sreturn %s%s."
           prepend_str
           (expr_to_html e)
           append_str, CNone
       | _, Val Val.Null ->
         sprintf
           "<li>%s%s%s.</li>"
           prepend_str
           (if prepend_str = "" then "Return" else "return")
           append_str, ctxt'
       | _ ->
         sprintf
           "<li>%s%s %s%s.</li>"
           prepend_str
           (if prepend_str = "" then "Return" else "return")
           (expr_to_html e)
           append_str, ctxt')

    | Throw e when ctxt' = Table ->
      sprintf
        "Throw %s."
        (expr_to_html e), CNone

    | Throw e ->
      let e_html = expr_to_html e in
      (match ctxt with
       | SameParagraph ->
         sprintf
           "%sthrow %s. %s"
           prepend_str
           e_html
           append_str, CNone
       | _ ->
         sprintf
           "<li>%s%s %s. %s</li>"
           prepend_str
           (if prepend_str = "" then "Throw" else "throw")
           e_html
           append_str, ctxt')

    | FieldAssign (e_o, f, e_v) ->
      let contents =
        sprintf
          "%s%s %s.%s to the value of %s. %s"
          prepend_str
          (if prepend_str = "" && not (ctxt' = SameParagraph) then "Set" else "set")
          (expr_to_html e_o)
          (expr_to_html f)
          (E_Expr.to_html Set e_v)
          append_str in
      (match ctxt' with
       | SameParagraph -> contents
       | _             -> "<li>" ^ contents ^ "</li>"
      ), ctxt'

    | FieldDelete (e, f) ->
      (match e with
       | Lookup (e', f') ->
         (match f' with
          | Val Str "JSProperties" ->
            sprintf
              "<li>%s%s the own property with name %s from %s. %s</li>"
              prepend_str
              (if prepend_str = "" then "Remove" else "remove")
              (expr_to_html f)
              (expr_to_html e')
              append_str
          | _ ->
            sprintf
              "<li>%s%s %s %s. %s<li>"
              prepend_str
              (if prepend_str = "" then "Remove" else "remove")
              (expr_to_html e)
              (expr_to_html f)
              append_str)
       | _ ->
         sprintf
           "<li>%s%s the binding for %s from %s. %s</li>"
           prepend_str
           (if prepend_str = "" then "Remove" else "remove")
           (expr_to_html f)
           (expr_to_html e)
           append_str), ctxt'

    | Block stmts ->
      let htmls, ctxt_block =
        List.fold_left (fun (htmls, ctxt_arg) s ->
            let s_html, ctxt' = to_html ~ctxt:ctxt_arg s in
            let ctxt'' =
              match ctxt' with
              | CNone -> ctxt
              | _     -> ctxt' in
            htmls @ [s_html], ctxt''
          ) ([], ctxt) stmts in
      String.concat "" htmls, ctxt_block

    | RepeatUntil (s, e) ->
      sprintf
        "<li>Repeat<ol class=\"block\">%s</ol></li>"
        (fst (to_html ~ctxt:ctxt' s)), ctxt'

    | While (e, s) ->
      sprintf
        "<li>Repeat while %s.<ol class=\"block\">%s</ol></li>"
        (expr_to_html e)
        (fst (to_html ~ctxt:ctxt' s)), ctxt'

    | ForEach (x, e, s, meta, var_meta) ->
      let var_prepend_str =
        match var_meta with
        | Some (s, alt) when s = x -> alt
        | _ -> "" in
      let after_same, next_ctxt = parse_if_meta meta ctxt in
      sprintf
        "<li>For each %s %s in %s%s<ol class=\"block\">%s</ol></li>"
        var_prepend_str
        (expr_to_html (Var x))
        (expr_to_html e)
        after_same
        (fst (to_html ~ctxt:ctxt' s)), ctxt'

    | If (e, s, None, _, _) when ctxt' = Table ->
      sprintf
        "The result is %s if the %s. "
        (fst (to_html ~ctxt:ctxt' s))
        (E_Expr.to_html Table e), ctxt'

    | If (e, s1, Some s2, _, _) when ctxt' = Table ->
      sprintf
        "The result is %s if the %s; otherwise the result is %s."
        (fst (to_html ~ctxt:ctxt' s1))
        (E_Expr.to_html Table e)
        (fst (to_html ~ctxt:ctxt' s2)), ctxt'

    | If (e, s, None, if_meta, _) when (is_split_if if_meta) && parse_if_split_meta if_meta = "" ->
      (* Ignore if_split_meta as it's expected to be empty, since this syntax is only supposed to appear at the middle of a block of statements *)
      sprintf
        "%s"
        (fst (to_html s)), ctxt'

    | If (e, s1, None, if_meta, _) when (is_split_if if_meta) ->
      let split_html = parse_if_split_meta if_meta in
      sprintf
        (* Since the function contents are wrapped in <ol> and this "if statement"
           with "split-if" metadata only occurs as the first statement of the function block,
           we only need to use this simple template in the sprintf *)
        "</ol>%s<ol class=\"proc\">%s"
        split_html
        (fst (to_html s1)), ctxt'

    | If (e, s, None, if_meta, _) when ctxt = SameParagraph ->
      let after_same, next_ctxt = parse_if_meta if_meta ctxt in
      sprintf
        "if %s, %s %s"
        (E_Expr.to_html IfGuard e)
        after_same
        (fst (to_html ~ctxt:SameParagraph s)), next_ctxt

    | If (e, s, None, if_meta, _) when is_basic_and_not_call s && swappable s ->
      sprintf
        "<li>%s, if %s.</li>"
        (fst (to_html ~ctxt:SameParagraph s))
        (E_Expr.to_html IfGuard e), ctxt'

    | If (e, s, None, if_meta, _) when is_basic_and_not_call s ->
      let after_same, next_ctxt = parse_if_meta if_meta ctxt in
      sprintf
        "<li>If %s, %s %s</li>"
        (E_Expr.to_html IfGuard e)
        after_same
        (fst (to_html ~ctxt:SameParagraph s)), next_ctxt

    | If (e, s, None, if_meta, _) ->
      let after_same, next_ctxt = parse_if_meta if_meta ctxt in
      sprintf
        "<li>If %s%s<ol class=\"block\">%s</ol></li>"
        (E_Expr.to_html IfGuard e)
        after_same
        (fst (to_html s)), next_ctxt

    | If (e, s1, Some s2, if_meta, else_meta) when is_let_stmt s1 && is_let_stmt s2 ->
      let after_same_if, _ = parse_if_meta if_meta ctxt in
      let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
      sprintf
        "<li>If %s, %s then %s, %s%s %s.</li>"
        (E_Expr.to_html IfGuard e)
        after_same_if
        (fst (to_html ~ctxt:SameParagraph s1))
        (if replace_str <> "" then replace_str else "else")
        after_same_else
        (fst (to_html ~ctxt:SameParagraph s2)), ctxt'

    | If (e, s1, Some s2, if_meta, else_meta) when is_basic_and_not_call s1 && is_basic_and_not_call s2 ->
      let after_same_if, _ = parse_if_meta if_meta ctxt in
      let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
      sprintf
        "<li>If %s, %s %s</li><li>%s%s %s</li>"
        (E_Expr.to_html IfGuard e)
        after_same_if
        (fst (to_html ~ctxt:SameParagraph s1))
        (if replace_str <> "" then replace_str else "Else")
        after_same_else
        (fst (to_html ~ctxt:SameParagraph s2)), next_ctxt

    | If (e, s1, Some s2, if_meta, else_meta) when is_basic_and_not_call s2 && not (has_after_same_meta else_meta) ->
      let after_same_if, _ = parse_if_meta if_meta ctxt in
      let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
      sprintf
        "<li>If %s%s<ol class=\"block\">%s</ol></li><li>%s%s %s</li>"
        (E_Expr.to_html IfGuard e)
        after_same_if
        (fst (to_html s1))
        (if replace_str <> "" then replace_str else "Else")
        after_same_else
        (fst (to_html ~ctxt:SameParagraph s2)), next_ctxt

    | If (e, s1, Some s2, if_meta, else_meta) when is_basic_and_not_call s1 && not (is_special_assign_expr s1) ->
      let after_same_if, _ = parse_if_meta if_meta ctxt in
      let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
      sprintf
        "<li>If %s, %s %s</li><li>%s%s<ol class=\"block\">%s</ol></li>"
        (E_Expr.to_html IfGuard e)
        after_same_if
        (fst (to_html ~ctxt:SameParagraph s1))
        (if replace_str <> "" then replace_str else "Else")
        after_same_else
        (fst (to_html s2)), next_ctxt

    | If (e, s1, Some s2, if_meta, else_meta) ->
      let after_same_if, _ = parse_if_meta if_meta ctxt in
      let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
      sprintf
        "<li>If %s%s<ol class=\"block\">%s</ol></li><li>%s%s<ol class=\"block\">%s</ol></li>"
        (E_Expr.to_html IfGuard e)
        after_same_if
        (fst (to_html s1))
        (if replace_str <> "" then replace_str else "Else")
        after_same_else
        (fst (to_html s2)), next_ctxt

    | EIf (ifs, final_else) ->
      let (e, s, m) = List.hd ifs in
      let ifs' = List.tl ifs in
      let first_if_html =
        match is_basic_and_not_call s with
        | false ->
          sprintf
            "<li>If %s%s<ol class=\"block\">%s</ol></li>"
            (E_Expr.to_html IfGuard e)
            (fst (parse_if_meta m ctxt))
            (fst (to_html s))
        | true ->
          sprintf
            "<li>If %s, %s %s</li>"
            (E_Expr.to_html IfGuard e)
            (fst (parse_if_meta m ctxt))
            (fst (to_html ~ctxt:SameParagraph s)) in
      let other_ifs_html =
        String.concat ""
          (List.map (
              fun (e, s, m) ->
                match is_basic_and_not_call s with
                | false ->
                  sprintf
                    "<li>Else, if %s%s<ol class=\"block\">%s</ol></li>"
                    (E_Expr.to_html IfGuard e)
                    (fst (parse_if_meta m ctxt))
                    (fst (to_html s))
                | true ->
                  sprintf
                    "<li>Else, if %s, %s %s</li>"
                    (E_Expr.to_html IfGuard e)
                    (fst (parse_if_meta m ctxt))
                    (fst (to_html ~ctxt:SameParagraph s))
            ) ifs') in
      let final_else_html =
        (match final_else with
         | None -> ""
         | Some (s, m) ->
           sprintf
             "<li>Else%s<ol class=\"block\">%s</ol></li>"
             (fst (parse_if_meta m ctxt))
             (fst (to_html s))
        ) in
      first_if_html ^ other_ifs_html ^ final_else_html, ctxt

    | Switch (e, es_ss, sd, meta) ->
      let table_caption = meta in
      let contain_ifs (stmts : t list) : bool =
        List.exists (fun s -> match s with If _ -> true | _ -> false) stmts in
      let cell_data (s : t) =
        match s with
        | Block stmts when not (contain_ifs stmts) ->
          sprintf
            "<p>Apply the following steps:</p><ol class=\"proc\">%s</ol>"
            (fst (to_html s))
        | _  ->
          sprintf
            "%s"
            (fst (to_html ~ctxt:Table s)) in
      let rows = List.map (
          fun (e, s) ->
            sprintf
              "<tr><td>%s</td>
                <td>%s</td>
              </tr>"
              (expr_to_html e)
              (cell_data s)
        ) es_ss in
      sprintf
        "<figure>
          <figcaption>%s</figcaption>
          <table class=\"real-table\">
            <tbody><tr>
              <th style=\"border-bottom: 1px solid #000000; border-left: 1px solid #000000; border-top: 2px solid #000000\">%s</th>
              <th style=\"border-bottom: 1px solid #000000; border-right: 1px solid #000000; border-top: 2px solid #000000\">Result</th>
            </tr>
            %s
          </tbody></table>
        </figure>"
        table_caption
        (E_Expr.to_html Table e)
        (String.concat "" rows), Table

    | Lambda _ -> "", ctxt'

end

module E_Func = struct
  include E_Func


  let is_descendent_of (f1 : t) (f2 : t) : bool =
    let f1_metadata = get_metadata f1 in
    let f2_metadata = get_metadata f2 in
    match f1_metadata, f2_metadata with
    | Some m1, Some m2 ->
      let f1_sec_number = E_Func_Metadata.get_section_number m1 in
      let f2_sec_number = E_Func_Metadata.get_section_number m2 in
      let f2_sub_sec_number = String.sub f2_sec_number 0 (String.length f1_sec_number) in
      (* We don't want to have E_Func with the same section_number as descendents *)
      f1_sec_number <> f2_sec_number && f2_sub_sec_number = f1_sec_number
    | _ -> invalid_arg "Error in \"HTMLExtensions.is_descendent_of\": at least one of the arguments doesn't have metadata."


  let to_html ?(inner_sections="") (f : t) : string =
    let e_func_metadata = get_metadata f in
    match e_func_metadata with
    | None      -> invalid_arg "Exception when executing to_html in a function without metadata."
    | Some meta ->
      let params_meta = E_Func_Metadata.get_meta_params meta in
      HTMLHashtables.E_Expr.init_var_bindings params_meta;
      let metadata_pre = E_Func_Metadata.get_pre meta in
      let metadata_post = E_Func_Metadata.get_post meta in
      let metadata_sec_number = E_Func_Metadata.get_section_number meta in
      let metadata_sec_name = E_Func_Metadata.get_section_name meta in
      let pre_note =
        match metadata_pre with
        | ""  -> ""
        | pre -> "<p>" ^ pre ^ "</p>" in
      let post_note =
        match metadata_post with
        | ""   -> ""
        | post -> "<p>" ^ post ^ "</p>" in
      let header =
        match metadata_sec_name with
        | None   -> ""
        | Some s ->
          (let sec_name =
             match s with
             | "" -> get_name f
             | _  -> s in
           let func_params =
             match get_params f with
             | [] -> ""
             | ps ->
               Printf.sprintf
                 "(%s)"
                 (String.concat ", " ps) in
           sprintf
             "<h1>
                <span class=\"secnum\">
                  <a href=\"#sec-%s\" title=\"link to this section\">%s</a>
                </span>
                %s %s
              </h1>"
             metadata_sec_number
             metadata_sec_number
             sec_name
             func_params
          ) in
      let body, ctxt = E_Stmt.to_html (get_body f) in
      let body' =
        match ctxt with
        | Table
        | MatchWith -> body
        | _         -> sprintf "<ol class=\"proc\">%s</ol>" body in
      sprintf
        "<section id=\"sec-%s\">%s%s%s%s%s</section>"
        metadata_sec_number
        header
        pre_note
        body'
        post_note
        inner_sections
end

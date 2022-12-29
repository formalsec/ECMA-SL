open Printf

module Val = struct
  include Val
  open HTMLHashtables.Val

  type ctxt_t = None | NoMatch | Negative

  let to_html ?(ctxt : ctxt_t = None) (v : t) : string =
    let wrap_in_b (s : string) = sprintf "<b>%s</b>" s in
    match v with
    | Void -> ""
    | Flt _ -> (
        let flt = Val.str ~flt_with_dot:false v in
        match (ctxt, flt) with
        | Negative, "0" ->
            "<span class=\"symbol\"><b>−</b></span>" ^ wrap_in_b "0"
        | None, "0" -> wrap_in_b "+0"
        | None, "nan" -> wrap_in_b "NaN"
        | Negative, "inf" ->
            "<span class=\"symbol\"><b>−</b></span>" ^ wrap_in_b "Infinity"
        | None, "inf" -> wrap_in_b "+Infinity"
        | Negative, _ -> "- " ^ wrap_in_b flt
        | _ -> wrap_in_b flt)
    | Int _ | Bool _ -> wrap_in_b (Val.str v)
    | Str v when ctxt = NoMatch -> sprintf "\"<code>%s</code>\"" (wrap_in_b v)
    | Str v -> (
        match Hashtbl.find_opt val_hashtable_html v with
        | None -> sprintf "\"<code>%s</code>\"" (wrap_in_b v)
        | Some s -> s)
    | Null -> ""
    | Symbol s -> wrap_in_b s
    | _ -> invalid_arg ("Invalid argument passed to Val.to_html: " ^ str v)
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
    | Assume
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
    | GVar v -> (
        match v with
        | "global" -> "the <a href=\"#sec-15.1\">global object</a>"
        | _ -> "")
    | Const _ -> ""
    | Val v -> (
        match (ctxt, v) with
        | Call, Val.Str s -> s
        | Negative, Val.Flt _ -> Val.to_html ~ctxt:Val.Negative v
        | _ -> Val.to_html v)
    | Var x -> (
        let x' = Hashtbl.find_opt vars_hashtable_html x in
        match (ctxt, x') with
        | _, Some s -> s
        | Negative, None -> sprintf "the result of negating <i>%s</i>" x
        | _, None -> "<i>" ^ x ^ "</i>")
    | UnOpt (op, e) -> (
        let op_str = Operators.str_of_unopt op in
        let f_op = Hashtbl.find_opt unoper_hashtable_html op_str in
        let res_op =
          Option.map_default (fun f -> f ctxt op_str e (Val Val.Null)) None f_op
        in
        match res_op with
        | Some s -> s
        | None ->
            let e_html = to_html ctxt e in
            sprintf "%s %s" op_str e_html)
    (* | BinOpt (Operators.Log_And, e1, e2) ->
       let e1_html = (to_html TopLevel e1) in
       let e2_html = (to_html TopLevel e2) in
       let subst = Hashtbl.create 0 in
       let str =
        if pattern_match subst e1 e2
        then sprintf "both %s and %s" e1_html e2_html
        else sprintf "%s and %s" e1_html e2_html in
       (match ctxt with
       | Negative -> "it is not the case that " ^ str
       | _ -> "it is the case that " ^ str) *)
    | BinOpt (op, e1, e2) -> (
        let op_str = Operators.str_of_binopt_single op in
        let default () =
          let f_op = Hashtbl.find_opt binoper_hashtable_html op_str in
          let res_op =
            Option.map_default (fun f -> f ctxt op_str e1 e2) None f_op
          in
          match res_op with
          | Some s -> s
          | None ->
              let e1_html = to_html BinaryExpr e1 in
              let e2_html = to_html BinaryExpr e2 in
              sprintf "%s %s %s" e1_html op_str e2_html
        in
        let expr_to_str (idx : int) : string =
          match idx with
          | 0 -> to_html BinaryExpr e1
          | 1 -> to_html BinaryExpr e2
          | _ -> invalid_arg "Invalid index"
        in
        if ctxt = Call then default ()
        else
          match HTML_Rules.apply_oper_rule_by_name op_str expr_to_str with
          | Some str -> str
          | None -> default ())
    | EBinOpt (op, e1, e2) -> (
        let op_str = EOper.str_of_binopt_single op in
        let expr_to_str (idx : int) : string =
          match idx with
          | 0 -> to_html BinaryExpr e1
          | 1 -> to_html BinaryExpr e2
          | _ -> invalid_arg "Invalid index"
        in
        match HTML_Rules.apply_oper_rule_by_name op_str expr_to_str with
        | Some str -> str
        | None -> (
            let f_op = Hashtbl.find_opt binoper_hashtable_html op_str in
            let res_op =
              Option.map_default (fun f -> f ctxt op_str e1 e2) None f_op
            in
            match res_op with
            | Some s -> s
            | None ->
                let e1_html = to_html BinaryExpr e1 in
                let e2_html = to_html BinaryExpr e2 in
                sprintf "%s %s %s" e1_html op_str e2_html))
    | NOpt (op, es) -> (
        match (op, es) with
        | Operators.ListExpr, [] -> "an empty <a href=\"#sec-8.8\">List</a>"
        | Operators.ListExpr, [ e1 ] ->
            sprintf "a <a href=\"#sec-8.8\">List</a> whose sole item is %s"
              (to_html ctxt e1)
        | Operators.ListExpr, [ e1; e2 ] ->
            sprintf "the pair (a two element List) consisting of %s and %s"
              (to_html ctxt e1) (to_html ctxt e2)
        | _, _ ->
            invalid_arg
              ("Invalid n-ary operator passed to E_Expr.to_html: "
              ^ Operators.str_of_nopt op (List.map str es)))
    | Call (f, es, Some g) ->
        let es_idx_to_html (idx : int) : string = aux (List.nth es idx) in
        let f' = to_html Call f in
        let f_html =
          match HTML_Rules.apply_func_call_rule_by_name f' es_idx_to_html with
          | Some str -> str
          | None ->
              invalid_arg
                ("Error: expecting HTML rule for function call \"" ^ f' ^ "\"")
        in
        let guard_html =
          match HTML_Rules.apply_func_call_rule_by_name g es_idx_to_html with
          | Some str -> str
          | None ->
              invalid_arg
                ("Error: expecting HTML rule for function call \"" ^ g ^ "\"")
        in
        sprintf "%s %s" f_html guard_html
    | Call (f, es, None) -> (
        let es_idx_to_html (idx : int) : string =
          to_html Call (List.nth es idx)
        in
        match ctxt with
        | IfGuard -> (
            let f' = match f with Val (Val.Str f) -> f | _ -> aux f in
            match HTML_Rules.apply_func_call_rule_by_name f' es_idx_to_html with
            | Some str -> str
            | None -> (
                let f_f' = Hashtbl.find_opt call_hashtable_html f' in
                let res_f' =
                  Option.map_default (fun fn -> fn ctxt f es) None f_f'
                in
                match res_f' with
                | Some s -> s
                | None ->
                    sprintf "%s(%s)" (aux f)
                      (String.concat ", " (List.map aux es))))
        | _ -> (
            let f' = to_html Call f in
            match HTML_Rules.apply_func_call_rule_by_name f' es_idx_to_html with
            | Some str -> str
            | None -> (
                let f_f' = Hashtbl.find_opt call_hashtable_html f' in
                let res_f' =
                  Option.map_default (fun fn -> fn ctxt f es) None f_f'
                in
                match res_f' with
                | Some s -> s
                | None ->
                    let args_str =
                      match es with
                      | [] -> "no arguments"
                      | e :: [] -> aux e ^ " as argument"
                      | es' ->
                          let rev_es = List.rev es' in
                          let last_e = List.hd rev_es in
                          let rest_es = List.tl rev_es in
                          sprintf "%s, and %s as arguments"
                            (String.concat ", " (List.rev_map aux rest_es))
                            (aux last_e)
                    in
                    sprintf "the result of calling %s passing %s" f' args_str)))
    | Lookup (e, f) -> (
        let f' = str f in
        match ctxt with
        | Call -> f'
        | _ -> (
            let strip_quotes (str : string) : string =
              String.concat "" (String.split_on_char '"' str)
            in
            match
              HTML_Rules.apply_lookup_rule_by_name (strip_quotes f') (fun () ->
                  aux e)
            with
            | Some str -> str
            | None -> (
                let f'' = Hashtbl.find_opt lookup_hashtable_html f' in
                match f'' with
                | None -> sprintf "the %s property of %s" (aux f) (aux e)
                | Some fn -> fn ctxt e f)))
    | NewObj fes ->
        sprintf "{%s}"
          (String.concat ", "
             (List.map
                (fun (f, e) -> sprintf "%s: %s" (aux (Val (Val.Str f))) (aux e))
                fes))
    | _ -> invalid_arg ("Unexpected argument passed to E_Expr.to_html: " ^ str e)

  let lookup_html_attribute (attribute_name : string) (ctxt : ctxt_t) (e : t)
      (f : t) : string =
    let e_html = to_html ctxt e in
    match (attribute_name, ctxt) with
    | _, Set -> sprintf "%s's [[%s]] attribute" e_html attribute_name
    | "Get", _ | "Put", _ ->
        sprintf "the [[%s]] internal method of %s" attribute_name e_html
    | _ -> sprintf "%s.[[%s]]" e_html attribute_name

  let call_html_type (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match (ctxt, es) with
    | Table, [ Var v ] -> Some (sprintf "%s Type" (String.capitalize_ascii v))
    | _, [ e ] ->
        Some (sprintf "<a href=\"#sec-8\">Type</a>(%s)" (to_html TopLevel e))
    | _ -> None

  let call_html_conversion_abstract_operations (oper_name : string)
      (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match (oper_name, es) with
    | "ToObject", [ Var "this" ] -> None
    | "ToObject", [ e ] ->
        Some
          (sprintf "<a href=\"#sec-9.9\">ToObject</a>(%s)" (to_html TopLevel e))
    | _ -> None

  let call_html_internal_method ?(print_brackets = true) (method_name : string)
      (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match (ctxt, method_name, es) with
    | ExprStmt, "put", [ e_this; e_prop; e_value; e_strict ] ->
        Some
          (sprintf
             "Call the <i>put</i> internal method using %s as its <b>this</b> \
              value, and passing %s for the property name, %s for the value, \
              and %s for the <i>Throw</i> flag"
             (to_html TopLevel e_this) (to_html TopLevel e_prop)
             (to_html TopLevel e_value)
             (to_html TopLevel e_strict))
    | ExprStmt, "Put", [ e_o; e_prop; e_value; e_strict ] ->
        Some
          (sprintf
             "Call the [[Put]] internal method of %s, passing %s for the \
              property name, %s for the value, and %s for the <i>Throw</i> \
              flag"
             (to_html TopLevel e_o) (to_html TopLevel e_prop)
             (to_html TopLevel e_value)
             (to_html TopLevel e_strict))
    | _, "get", [ e_this; e_prop ] ->
        Some
          (sprintf
             "the result of calling the <i>get</i> internal method using %s as \
              its <b>this</b> value, and passing %s for the argument"
             (to_html TopLevel e_this) (to_html TopLevel e_prop))
    | _, "Construct", [ _; _; e_o; e_p ] ->
        Some
          (sprintf
             "the result of calling the [[Construct]] internal method of %s \
              providing %s as the arguments"
             (to_html TopLevel e_o) (to_html TopLevel e_p))
    | ExprStmt, _, [ e_o; e_f ] ->
        Some
          (sprintf
             "Call the %s internal method of %s passing %s as the argument"
             (if print_brackets then "[[" ^ method_name ^ "]]"
             else "<i>" ^ method_name ^ "</i>")
             (to_html TopLevel e_o) (to_html TopLevel e_f))
    | ExprStmt, _, [ e_o; e_f1; e_f2 ] ->
        Some
          (sprintf
             "Call the %s internal method of %s passing %s, and %s as arguments"
             (if print_brackets then "[[" ^ method_name ^ "]]"
             else "<i>" ^ method_name ^ "</i>")
             (to_html TopLevel e_o) (to_html TopLevel e_f1)
             (to_html TopLevel e_f2))
    | ExprStmt, _, [ e_o; e_f1; e_f2; e_f3 ] ->
        Some
          (sprintf
             "Call the %s internal method of %s passing %s, %s, and %s as \
              arguments"
             (if print_brackets then "[[" ^ method_name ^ "]]"
             else "<i>" ^ method_name ^ "</i>")
             (to_html TopLevel e_o) (to_html TopLevel e_f1)
             (to_html TopLevel e_f2) (to_html TopLevel e_f3))
    | _, _, [ e_o; e_f ] ->
        Some
          (sprintf
             "the result of calling the %s internal method of %s with argument \
              %s"
             (if print_brackets then "[[" ^ method_name ^ "]]"
             else "<i>" ^ method_name ^ "</i>")
             (to_html TopLevel e_o) (to_html TopLevel e_f))
    | _, _, [ e_o; e_f1; e_f2 ] ->
        Some
          (sprintf
             "the result of calling the %s internal method of %s with \
              arguments %s and %s"
             (if print_brackets then "[[" ^ method_name ^ "]]"
             else "<i>" ^ method_name ^ "</i>")
             (to_html TopLevel e_o) (to_html TopLevel e_f1)
             (to_html TopLevel e_f2))
    | _ -> None

  let call_html_call (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match (ctxt, es) with
    | ( ExprStmt,
        [ Val Val.Null; Val Val.Null; e_o; e_this; NOpt (Operators.ListExpr, []) ] )
      ->
        Some
          (sprintf
             "Call the [[Call]] internal method of %s providing %s as the \
              <b>this</b> value and providing no arguments"
             (to_html TopLevel e_o) (to_html TopLevel e_this))
    | ( ExprStmt,
        [ Val Val.Null; Val Val.Null; e_o; e_this; NOpt (Operators.ListExpr, [ e ]) ]
      ) ->
        Some
          (sprintf
             "Call the [[Call]] internal method of %s providing %s as the \
              <b>this</b> value and an argument list containing only %s"
             (to_html TopLevel e_o) (to_html TopLevel e_this)
             (to_html TopLevel e))
    | Let, [ Val Val.Null; Val Val.Null; e_o; e_this; NOpt (Operators.ListExpr, []) ]
      ->
        Some
          (sprintf
             "the result of calling the [[Call]] internal method of %s, with \
              %s as the this value and an empty argument list"
             (to_html TopLevel e_o) (to_html TopLevel e_this))
    | Let, [ _; _; e_o; e_this; e_args ] ->
        Some
          (sprintf
             "the result of calling the [[Call]] internal property of %s, \
              providing %s as the <b>this</b> value and providing the %s as \
              <i>args</i>"
             (to_html TopLevel e_o) (to_html TopLevel e_this)
             (to_html TopLevel e_args))
    | _, [ Val Val.Null; Val Val.Null; e_o; e_this; NOpt (Operators.ListExpr, []) ]
      ->
        Some
          (sprintf
             "the result of calling the [[Call]] internal method of %s \
              providing %s as the <b>this</b> value and providing no arguments"
             (to_html TopLevel e_o) (to_html TopLevel e_this))
    | ( _,
        [ Val Val.Null; Val Val.Null; e_o; e_this; NOpt (Operators.ListExpr, [ e ]) ]
      ) ->
        Some
          (sprintf
             "the result of calling the [[Call]] internal method of %s \
              providing %s as the <b>this</b> value and an argument list \
              containing only %s"
             (to_html TopLevel e_o) (to_html TopLevel e_this)
             (to_html TopLevel e))
    | _, [ Val Val.Null; Val Val.Null; e_o; e_this; NOpt (Operators.ListExpr, arr) ]
      ->
        Some
          (sprintf
             "the result of calling the [[Call]] internal method of %s \
              providing %s as the <b>this</b> value and an argument list \
              containing %s"
             (to_html TopLevel e_o) (to_html TopLevel e_this)
             (String.concat ", " (List.map (to_html TopLevel) arr)))
    | _, [ _; _; e_o; e_this; args_list ] ->
        Some
          (sprintf
             "the result of calling the [[Call]] internal method of %s \
              providing %s as the <b>this</b> value and %s as the list of \
              arguments"
             (to_html TopLevel e_o) (to_html TopLevel e_this)
             (to_html TopLevel args_list))
    | _ -> None

  let call_html_createmutablebinding_method (ctxt : ctxt_t) (e : t)
      (es : t list) : string option =
    match es with
    | [ e1; Val (Val.Str s); Val (Val.Bool false) ] ->
        Some
          (sprintf
             "Call %s’s CreateMutableBinding concrete method passing the \
              String %s as the argument"
             (to_html ctxt e1)
             (to_html ctxt (Val (Val.Str s))))
    | [ e1; e2; Val (Val.Bool false) ] ->
        Some
          (sprintf
             "Call %s’s CreateMutableBinding concrete method passing %s as the \
              argument"
             (to_html ctxt e1) (to_html ctxt e2))
    | [ e1; e2; e3 ] ->
        Some
          (sprintf
             "Call %s’s CreateMutableBinding concrete method passing %s and %s \
              as the arguments"
             (to_html ctxt e1) (to_html ctxt e2) (to_html ctxt e3))
    | _ -> None

  let call_html_createimmutablebinding_method (ctxt : ctxt_t) (e : t)
      (es : t list) : string option =
    match es with
    | [ e1; Val (Val.Str s) ] ->
        Some
          (sprintf
             "Call %s’s <a href=\"x10.2.1.1.7\">CreateImmutableBinding</a> \
              concrete method passing the String %s as the argument"
             (to_html ctxt e1)
             (to_html ctxt (Val (Val.Str s))))
    | [ e1; e2 ] ->
        Some
          (sprintf
             "Call %s’s <a href=\"x10.2.1.1.7\">CreateImmutableBinding</a> \
              concrete method passing %s as the argument"
             (to_html ctxt e1) (to_html ctxt e2))
    | _ -> None

  let call_html_abstractrelationalcomparison (ctxt : ctxt_t) (e : t)
      (es : t list) : string option =
    match es with
    | [ e1; e2; Val (Val.Bool true) ] ->
        Some
          (sprintf
             "the result of performing abstract relational comparison %s &lt; \
              %s. (<a href=\"#sec-11.8.5\">see 11.8.5</a>)"
             (to_html ctxt e1) (to_html ctxt e2))
    | [ e1; e2; Val (Val.Bool false) ] ->
        Some
          (sprintf
             "the result of performing abstract relational comparison %s &lt; \
              %s with <i>LeftFirst</i> equal to <b>false</b>. (<a \
              href=\"#sec-11.8.5\">see 11.8.5</a>)"
             (to_html ctxt e1) (to_html ctxt e2))
    | _ -> None

  let call_html_constructor (constructor : string) (ctxt : ctxt_t) (e : t)
      (es : t list) : string option =
    match (ctxt, es) with
    | Table, [ GVar _; _; _; NOpt (Operators.ListExpr, [ expr ]) ] ->
        let built_in, section_html =
          match e with
          | Val (Val.Str "BooleanConstructor") ->
              ("Boolean", "<a href=\"#sec-15.6\">15.6</a>")
          | Val (Val.Str "NumberConstructor") ->
              ("Number", "<a href=\"#sec-15.7\">15.7</a>")
          | Val (Val.Str "StringConstructor") ->
              ("String", "<a href=\"#sec-15.5\">15.5</a>")
          | _ -> invalid_arg ("Unexpected expression: " ^ str e)
        in
        Some
          (sprintf
             "Create a new %s object whose [[PrimitiveValue]] internal \
              property is set to the value of the argument. See %s for a \
              description of %s objects."
             built_in section_html built_in)
    | ( _,
        [
          GVar _;
          Val (Val.Symbol "null");
          _;
          NOpt (Operators.ListExpr, [ Val Val.Null ]);
        ] )
    | _, [ _; _; _; NOpt (Operators.ListExpr, []) ] ->
        let const_hmtl =
          match e with
          | Val (Val.Str "ObjectConstructor") ->
              "<a href=\"#sec-15.2\">the standard built-in constructor with \
               that name</a>"
          | _ -> "the standard built-in constructor with that name"
        in
        Some
          (sprintf
             "the result of creating a new object as if by the expression \
              <code><b>new %s()</b></code> where <code><b>%s</b></code> is %s"
             constructor constructor const_hmtl)
    | _, [ _; _; _; NOpt (Operators.ListExpr, [ e1 ]) ] ->
        let const_hmtl =
          match e with
          | Val (Val.Str "ObjectConstructor") ->
              "<a href=\"#sec-15.2\">the standard built-in constructor with \
               that name</a>"
          | _ -> "the standard built-in constructor with that name"
        in
        Some
          (sprintf
             "the result of creating a new object as if by the expression \
              <code><b>new %s(%s)</b></code> where <code><b>%s</b></code> is \
              %s"
             constructor (to_html ctxt e1) constructor const_hmtl)
    | _ -> None

  let call_html_objectdefineproperties_builtin_method (ctxt : ctxt_t) (e : t)
      (es : t list) : string option =
    match es with
    | [ _; _; _; NOpt (Operators.ListExpr, [ e1; e2 ]) ] ->
        Some
          (sprintf
             "Add own properties to %s as if by calling the standard built-in \
              function <code><b>Object.defineProperties</b></code> with \
              arguments %s and %s"
             (to_html ctxt e1) (to_html ctxt e1) (to_html ctxt e2))
    | _ -> None

  let call_html_iscallable (ctxt : ctxt_t) (e : t) (es : t list) : string option
      =
    match (es, ctxt) with
    | [ e ], BinaryExpr ->
        Some
          (sprintf "<a href=\"#sec-9.11\">IsCallable</a>(%s)"
             (to_html TopLevel e))
    | [ e ], _ -> Some (sprintf "%s is callable" (to_html TopLevel e))
    | _ -> None

  let call_html_getvalue (ctxt : ctxt_t) (e : t) (es : t list) : string option =
    match (ctxt, es) with
    | ExprStmt, [ e ] ->
        Some
          (sprintf "Call <a href=\"#sec-8.7.1\">GetValue</a>(%s)"
             (to_html TopLevel e))
    | _, [ e ] ->
        Some
          (sprintf "<a href=\"#sec-8.7.1\">GetValue</a>(%s)"
             (to_html TopLevel e))
    | _ -> None

  let call_html_dataproperty (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match (es, ctxt) with
    | [ e ], BinaryExpr ->
        Some
          (sprintf "<a href=\"#sec-8.10.2\">IsDataDescriptor</a>(%s)"
             (to_html TopLevel e))
    | [ e ], _ -> Some (sprintf "%s is a data property" (to_html TopLevel e))
    | _ -> None

  let call_html_accessorproperty (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match (es, ctxt) with
    | [ e ], BinaryExpr ->
        Some
          (sprintf "<a href=\"#sec-8.10.1\">IsAccessorDescriptor</a>(%s)"
             (to_html TopLevel e))
    | [ e ], _ ->
        Some (sprintf "%s is an accessor property" (to_html TopLevel e))
    | _ -> None

  let call_html_setbindinginitialised (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match es with
    | [ e1; e2; Val (Val.Bool false) ] ->
        Some
          (sprintf "Record that the binding for %s in %s is uninitialised"
             (to_html ctxt e2) (to_html ctxt e1))
    | [ e1; e2; Val (Val.Bool true) ] ->
        Some
          (sprintf
             "Record that the <a href=\"#immutable-binding\">immutable \
              binding</a> for %s in %s has been initialised"
             (to_html ctxt e2) (to_html ctxt e1))
    | _ -> None

  let call_html_setlexicalenvironment (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match es with
    | [ Var "scope"; e1 ] ->
        Some
          (sprintf
             "Set the running execution context’s <a \
              href=\"#sec-10.3\">LexicalEnvironment</a> to %s"
             (to_html ctxt e1))
    | [ _; e1 ] ->
        Some
          (sprintf "Set the <a href=\"#sec-10.3\">LexicalEnvironment</a> to %s"
             (to_html ctxt e1))
    | _ -> None

  let call_html_getlexicalenvironment (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match es with
    | [ Var "callingExecCtx" ] ->
        Some
          "the same value as the <a href=\"#sec-10.2\">LexicalEnvironment</a> \
           of the calling execution context"
    | [ Var "evalExecCtx" ] -> Some "the LexicalEnvironment"
    | _ ->
        Some
          "the running execution context‘s <a \
           href=\"#sec-10.2\">LexicalEnvironment</a>"

  let call_html_getvariableenvironment (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match es with
    | [ Var "callingExecCtx" ] ->
        Some
          "the same value as the VariableEnvironment of the calling execution \
           context"
    | _ ->
        Some
          "the running execution context‘s <a \
           href=\"#sec-10.3\">VariableEnvironment</a>"

  let call_html_declarationbindinginstantiation (ctxt : ctxt_t) (e : t)
      (es : t list) : string option =
    match es with
    | [ Var x; _; _; Val Val.Null ] ->
        Some
          (sprintf
             "Perform Declaration Binding Instantiation as described in <a \
              href=\"#sec-10.5\">10.5</a> using the %s"
             (to_html ctxt (Var x)))
    | [ e1; e2; _; _ ] ->
        Some
          (sprintf
             "Perform Declaration Binding Instantiation using the <a \
              href=\"#function-code\">function code</a> %s and %s as described \
              in <a href=\"#sec-10.5\">10.5</a>"
             (to_html ctxt e1) (to_html ctxt e2))
    | _ -> None

  let call_html_createfunctionobject (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match es with
    | [ NOpt (Operators.ListExpr, []); e1; e2; e3; _ ] ->
        Some
          (sprintf
             "the result of creating a function object as described in <a \
              href=\"#sec-13.2\">13.2</a> using no <i>FormalParameterList</i>, \
              %s for <i><a href=\"#sec-13\">FunctionBody</a></i>, %s as Scope, \
              and %s for <i>Strict</i>"
             (to_html ctxt e1) (to_html ctxt e2) (to_html ctxt e3))
    | [ NOpt (Operators.ListExpr, [ Val (Val.Str s) ]); e1; e2; e3; _ ] ->
        Some
          (sprintf
             "the result of creating a function object as described in <a \
              href=\"#sec-13.2\">13.2</a> using a <a \
              href=\"#sec-8.8\">List</a> containing the single String %s as \
              <i>FormalParameterList</i>, %s for <i><a \
              href=\"#sec-13\">FunctionBody</a></i>, %s as <i>Scope</i>, and \
              %s for <i>Strict</i>"
             (to_html ctxt (Var s)) (to_html ctxt e1) (to_html ctxt e2)
             (to_html ctxt e3))
    | [ e1; e2; e3; Call (Val (Val.Str "isStrictModeCode"), _, _); _ ] ->
        Some
          (sprintf
             "the result of creating a new Function object as specified in <a \
              href=\"#sec-13.2\">13.2</a> with parameters specified by %s, and \
              body specified by %s. Pass in %s as the <i>Scope</i>. Pass in \
              <b>true</b> as the <i>Strict</i> flag if the FunctionDeclaration \
              is contained in <a href=\"#sec-10.1.1\">strict code</a> or if \
              its <i>FunctionBody</i> is <a href=\"#sec-10.1.1\">strict \
              code</a>"
             (to_html ctxt e1) (to_html ctxt e2) (to_html ctxt e3))
    | [ e1; e2; e3; e4; _ ] ->
        Some
          (sprintf
             "a new Function object created as specified in <a \
              href=\"#sec-13.2\">13.2</a> passing %s as the \
              <i>FormalParameterList</i><sub>opt</sub> and %s as the \
              <i>FunctionBody</i>. Pass in %s as the <i>Scope</i> parameter \
              and %s as the <i>Strict</i> flag"
             (to_html ctxt e1) (to_html ctxt e2) (to_html ctxt e3)
             (to_html ctxt e4))
    | _ -> None

  let call_html_initialglobalexecutioncontext (ctxt : ctxt_t) (e : t)
      (es : t list) : string option =
    match es with
    | [ _; Var "evalCode" ] ->
        Some
          "Initialise the execution context as if it was a global execution \
           context using the <a href=\"#eval-code\">eval code</a> as <i>C</i> \
           as described in <a href=\"#sec-10.4.1.1\">10.4.1.1</a>"
    | _ ->
        Some
          "Initialise the execution context using the <a \
           href=\"#global-code\">global code</a> as described in <a \
           href=\"#sec-10.4.1.1\">10.4.1.1</a>"

  let call_html_isstrictmodecode (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match es with
    | [ Var "evalCode"; _ ] ->
        Some
          "the <a href=\"#eval-code\">eval code</a> is <a \
           href=\"#sec-10.1.1\">strict mode code</a>"
    | [ e; _ ] ->
        Some
          (sprintf "%s is <a href=\"#sec-10.1.1\">strict mode code</a>"
             (to_html ctxt e))
    | _ -> None

  let call_html_setinternalproperty (ctxt : ctxt_t) (e : t) (es : t list) :
      string option =
    match es with
    | [ obj; p; Val (Val.Str s) ] when is_special_val s ->
        Some
          (sprintf "Set the %s internal property of %s %s" (to_html ctxt p)
             (to_html ctxt obj) (get_special_val_html s))
    | [ obj; p; v ] ->
        Some
          (sprintf "Set the %s internal property of %s to %s" (to_html ctxt p)
             (to_html ctxt obj) (to_html ctxt v))
    | _ -> None

  let oper_html_lnth (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) :
      string option =
    match e2 with
    | BinOpt (Operators.Minus, x, Val (Val.Int 1)) ->
        Some
          (sprintf "the value of the %s'th element of %s" (to_html ctxt x)
             (to_html ctxt e1))
    | Var _ ->
        Some
          (sprintf "the element of %s at 0-origined list position %s"
             (to_html ctxt e1) (to_html ctxt e2))
    | Val (Val.Int 0) ->
        Some (sprintf "the first element of %s" (to_html ctxt e1))
    | Val (Val.Int 1) ->
        Some (sprintf "the second element of %s" (to_html ctxt e1))
    | _ -> None

  let oper_html_inobj (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) :
      string option =
    match (e1, e2, ctxt) with
    | _, Lookup (e2', Val (Val.Str "JSProperties")), Negative ->
        Some
          (sprintf "%s doesn’t have an own property with name %s"
             (to_html TopLevel e2') (to_html TopLevel e1))
    | _, Lookup (e2', Val (Val.Str "JSProperties")), _ ->
        Some
          (sprintf "%s has a property with name %s" (to_html ctxt e2')
             (to_html ctxt e1))
    | _, Var "envRec", Negative ->
        Some
          (sprintf
             "%s does not have a binding for the name that is the value of %s"
             (to_html TopLevel e2) (to_html TopLevel e1))
    | _, Var "envRec", AssertNegative ->
        Some
          (sprintf "%s does not already have a binding for %s"
             (to_html TopLevel e2) (to_html TopLevel e1))
    | _, Var "envRec", Assert ->
        Some
          (sprintf "%s has a binding for %s" (to_html TopLevel e2)
             (to_html TopLevel e1))
    | _, Var "envRec", _ ->
        Some
          (sprintf "%s has a binding for the name that is the value of %s"
             (to_html ctxt e2) (to_html ctxt e1))
    | Val (Val.Str x), Var _, Negative when is_internal_method x ->
        Some
          (sprintf "%s has no [[%s]] internal method" (to_html TopLevel e2) x)
    | Val (Val.Str x), Var _, _ when is_internal_method x ->
        Some (sprintf "%s has a [[%s]] internal method" (to_html ctxt e2) x)
    | _, Var _, Negative ->
        Some
          (sprintf "%s.%s is absent" (to_html TopLevel e2) (to_html TopLevel e1))
    | _, Var _, _ ->
        Some (sprintf "%s.%s is present" (to_html ctxt e2) (to_html ctxt e1))
    | _ -> None

  let oper_html_inlist (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) :
      string option =
    let ctxt_str = match ctxt with Negative -> "is not" | _ -> "is" in
    Some
      (sprintf "%s %s an element of %s" (to_html BinaryExpr e1) ctxt_str
         (to_html BinaryExpr e2))

  let oper_html_equal (ctxt : ctxt_t) (op : string) (e1 : t) (e2 : t) :
      string option =
    match (e2, ctxt) with
    | GVar "global", _ ->
        Some
          (sprintf
             "%s is the <a href=\"#sec-10.2\">environment record</a> component \
              of the global environment"
             (to_html BinaryExpr e1))
    | Val Val.Null, Negative ->
        Some (sprintf "%s is supplied" (to_html BinaryExpr e1))
    | Val Val.Null, _ ->
        Some (sprintf "%s is not supplied" (to_html BinaryExpr e1))
    | _, Negative ->
        Some
          (sprintf "%s is not %s" (to_html BinaryExpr e1)
             (to_html BinaryExpr e2))
    | _ ->
        Some
          (sprintf "%s is %s" (to_html BinaryExpr e1) (to_html BinaryExpr e2))

  let oper_html_unary_neg (ctxt : ctxt_t) (op : string) (e : t) _ :
      string option =
    match (ctxt, op) with
    | Assert, "!" -> Some (to_html AssertNegative e)
    | _, "!" | _, "-" -> Some (to_html Negative e)
    | _ -> None

  let oper_html_unary_len (ctxt : ctxt_t) (op : string) (e : t) _ :
      string option =
    match op with
    | "l_len" -> Some (sprintf "the number of elements in %s" (to_html ctxt e))
    | _ -> None

  let oper_html_sconcat (ctxt : ctxt_t) (op : string) (e : t) _ : string option
      =
    match e with
    | NOpt (Operators.ListExpr, [ e1; e2; e3 ]) ->
        Some
          (sprintf
             "the String value that is the result of concatenating the three \
              Strings %s, %s, and %s"
             (to_html ctxt e1) (to_html ctxt e2) (to_html ctxt e3))
    | NOpt (Operators.ListExpr, es) ->
        Some
          (sprintf "the String that is the result of concatenating %s"
             (match es with
             | [] ->
                 invalid_arg
                   "Exception in oper_html_sconcat: unexpected empty list."
             | e :: [] -> to_html ctxt e
             | es' ->
                 let rev_es = List.rev es' in
                 let last_e = List.hd rev_es in
                 let rest_es = List.tl rev_es in
                 sprintf "%s, and %s"
                   (String.concat ", " (List.rev_map (to_html ctxt) rest_es))
                   (to_html ctxt last_e)))
    | _ -> None

  let oper_html_bitwisenot (ctxt : ctxt_t) (op : string) (e : t) _ :
      string option =
    Some
      (sprintf
         "the result of applying bitwise complement to %s. The result is a \
          signed 32-bit integer"
         (to_html ctxt e))

  let oper_hmtl_print_ignore (ctxt : ctxt_t) (op : string) (e : t) _ :
      string option =
    Some (to_html ctxt e)

  let () =
    Hashtbl.add binoper_hashtable_html
      (Operators.str_of_binopt_single Operators.InObj)
      oper_html_inobj;
    Hashtbl.add binoper_hashtable_html
      (Operators.str_of_binopt_single Operators.InList)
      oper_html_inlist;
    Hashtbl.add binoper_hashtable_html
      (Operators.str_of_binopt_single Operators.Lnth)
      oper_html_lnth;
    Hashtbl.add binoper_hashtable_html
      (Operators.str_of_binopt_single Operators.Equal)
      oper_html_equal;
    Hashtbl.add unoper_hashtable_html
      (Operators.str_of_unopt Operators.IntToFloat)
      oper_hmtl_print_ignore;
    Hashtbl.add unoper_hashtable_html
      (Operators.str_of_unopt Operators.Neg)
      oper_html_unary_neg;
    Hashtbl.add unoper_hashtable_html
      (Operators.str_of_unopt Operators.Not)
      oper_html_unary_neg;
    Hashtbl.add unoper_hashtable_html
      (Operators.str_of_unopt Operators.ListLen)
      oper_html_unary_len;
    Hashtbl.add unoper_hashtable_html
      (Operators.str_of_unopt Operators.Sconcat)
      oper_html_sconcat;
    Hashtbl.add unoper_hashtable_html
      (Operators.str_of_unopt Operators.BitwiseNot)
      oper_html_bitwisenot;
    Hashtbl.add lookup_hashtable_html "\"Value\""
      (lookup_html_attribute "Value");
    Hashtbl.add lookup_hashtable_html "\"Writable\""
      (lookup_html_attribute "Writable");
    Hashtbl.add lookup_hashtable_html "\"Set\"" (lookup_html_attribute "Set");
    Hashtbl.add lookup_hashtable_html "\"Get\"" (lookup_html_attribute "Get");
    Hashtbl.add lookup_hashtable_html "\"Put\"" (lookup_html_attribute "Put");
    Hashtbl.add lookup_hashtable_html "\"Enumerable\""
      (lookup_html_attribute "Enumerable");
    Hashtbl.add lookup_hashtable_html "\"Configurable\""
      (lookup_html_attribute "Configurable");
    Hashtbl.add call_hashtable_html "\"Call\"" call_html_call;
    Hashtbl.add call_hashtable_html "\"GetOwnProperty\""
      (call_html_internal_method "GetOwnProperty");
    Hashtbl.add call_hashtable_html "\"GetProperty\""
      (call_html_internal_method "GetProperty");
    Hashtbl.add call_hashtable_html "\"Get\"" (call_html_internal_method "Get");
    Hashtbl.add call_hashtable_html "<i>get</i>"
      (call_html_internal_method ~print_brackets:false "get");
    Hashtbl.add call_hashtable_html "\"CanPut\""
      (call_html_internal_method "CanPut");
    Hashtbl.add call_hashtable_html "\"Put\"" (call_html_internal_method "Put");
    Hashtbl.add call_hashtable_html "Put" (call_html_internal_method "Put");
    Hashtbl.add call_hashtable_html "<i>put</i>"
      (call_html_internal_method ~print_brackets:false "put");
    Hashtbl.add call_hashtable_html "\"HasProperty\""
      (call_html_internal_method "HasProperty");
    Hashtbl.add call_hashtable_html "\"Delete\""
      (call_html_internal_method "Delete");
    Hashtbl.add call_hashtable_html "\"DefaultValue\""
      (call_html_internal_method "DefaultValue");
    Hashtbl.add call_hashtable_html "\"DefineOwnProperty\""
      (call_html_internal_method "DefineOwnProperty");
    Hashtbl.add call_hashtable_html "\"HasInstance\""
      (call_html_internal_method "HasInstance");
    Hashtbl.add call_hashtable_html "\"Construct\""
      (call_html_internal_method "Construct");
    Hashtbl.add call_hashtable_html "ObjectConstructor"
      (call_html_constructor "Object");
    Hashtbl.add call_hashtable_html "BooleanConstructor"
      (call_html_constructor "Boolean");
    Hashtbl.add call_hashtable_html "NumberConstructor"
      (call_html_constructor "Number");
    Hashtbl.add call_hashtable_html "StringConstructor"
      (call_html_constructor "String");
    Hashtbl.add call_hashtable_html "ArrayConstructor"
      (call_html_constructor "Array");
    Hashtbl.add call_hashtable_html "ObjectDefineProperties"
      call_html_objectdefineproperties_builtin_method;
    Hashtbl.add call_hashtable_html "Type" call_html_type;
    Hashtbl.add call_hashtable_html "GetValue" call_html_getvalue;
    Hashtbl.add call_hashtable_html "ToObject"
      (call_html_conversion_abstract_operations "ToObject");
    Hashtbl.add call_hashtable_html "IsCallable" call_html_iscallable;
    Hashtbl.add call_hashtable_html "IsDataPropertyDescriptor"
      call_html_dataproperty;
    Hashtbl.add call_hashtable_html "IsAccessorPropertyDescriptor"
      call_html_accessorproperty;
    Hashtbl.add call_hashtable_html "CreateMutableBinding"
      call_html_createmutablebinding_method;
    Hashtbl.add call_hashtable_html "CreateImmutableBinding"
      call_html_createimmutablebinding_method;
    Hashtbl.add call_hashtable_html "InitialGlobalExecutionContext"
      call_html_initialglobalexecutioncontext;
    Hashtbl.add call_hashtable_html "DeclarationBindingInstantiation"
      call_html_declarationbindinginstantiation;
    Hashtbl.add call_hashtable_html "CreateFunctionObject"
      call_html_createfunctionobject;
    Hashtbl.add call_hashtable_html "AbstractRelationalComparison"
      call_html_abstractrelationalcomparison;
    Hashtbl.add call_hashtable_html "setBindingInitialised"
      call_html_setbindinginitialised;
    Hashtbl.add call_hashtable_html "setLexicalEnvironment"
      call_html_setlexicalenvironment;
    Hashtbl.add call_hashtable_html "getLexicalEnvironment"
      call_html_getlexicalenvironment;
    Hashtbl.add call_hashtable_html "getVariableEnvironment"
      call_html_getvariableenvironment;
    Hashtbl.add call_hashtable_html "isStrictModeCode"
      call_html_isstrictmodecode;
    Hashtbl.add call_hashtable_html "setInternalProperty"
      call_html_setinternalproperty
end

module E_Stmt = struct
  include E_Stmt
  open HTMLHashtables.E_Stmt

  type ctxt_t =
    | CNone
    | Prepend of string
    | Append of string
    | Replace of string
    | SameParagraph
    | Table
    | MatchWith

  let is_split_if (meta : metadata_t list) : bool =
    try
      let m = List.hd meta in
      match m.where with "split-if" -> true | _ -> false
    with Failure _ -> false

  let parse_if_split_meta (meta : metadata_t list) : string =
    try
      let m = List.hd meta in
      match m.where with "split-if" -> m.html | _ -> ""
    with Failure _ -> ""

  let parse_if_meta (meta : metadata_t list) (ctxt : ctxt_t) : string * ctxt_t =
    List.fold_left
      (fun acc m ->
        match m.where with
        | "before-same" -> (fst acc, Prepend m.html)
        | "after-same" -> (m.html, snd acc)
        | "after-next" -> (fst acc, Append m.html)
        | "replace-with" -> (fst acc, Replace m.html)
        | _ -> acc)
      ("", ctxt) meta

  let has_after_same_meta (meta : metadata_t list) : bool =
    List.exists (fun m -> m.where = "after-same") meta

  let is_basic_if (s : t) : bool =
    is_basic s
    || match s with If (e, s1, None, _, _) -> is_basic s1 | _ -> false

  let is_special_expr (e : E_Expr.t) : bool =
    match e with
    | E_Expr.Call (E_Expr.Val (Val.Str "InitialGlobalExecutionContext"), _, _)
    | E_Expr.BinOpt (Operators.Ladd, _, _) ->
        true
    | _ -> false

  let is_basic_and_not_call (s : t) : bool =
    is_basic_if s
    &&
    match s with
    | ExprStmt e -> ( match e with E_Expr.Call _ -> false | _ -> true)
    | _ -> true

  let is_special_assign_expr (s : t) : bool =
    match s with Assign (_, e) when is_special_expr e -> true | _ -> false

  let is_let_stmt (s : t) : bool =
    match s with Assign (x, e) -> true | _ -> false

  let swappable (s : t) : bool =
    match s with
    | MacroApply (f, _) -> Hashtbl.mem swappable_hashtable_html f
    | _ -> false

  let rec to_html ?(ctxt = CNone) (stmt : t) : string * ctxt_t =
    let expr_to_html = E_Expr.(to_html TopLevel) in
    let ctxt', prepend_str, append_str, replace_str =
      match ctxt with
      | CNone | SameParagraph | Table | MatchWith -> (ctxt, "", "", "")
      | Prepend s -> (CNone, s, "", "")
      | Append s -> (CNone, "", s, "")
      | Replace s -> (CNone, "", "", s)
    in
    match stmt with
    | Skip | Print _ | GlobAssign _ | Fail _ -> ("", ctxt')
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
    | Assume e ->
        let e_html = E_Expr.(to_html Assert e) in
        (sprintf "<li>Assert: %s.</li>" e_html, ctxt')
    | Assert e ->
        let e_html = E_Expr.(to_html Assume e) in
        (sprintf "<li>Assert: %s.</li>" e_html, ctxt')
    | Wrapper (meta, s) -> (
        let m = List.hd meta in
        match m.where with
        | "before-same" -> to_html ~ctxt:(Prepend m.html) s
        | "after-same" -> to_html ~ctxt:(Append m.html) s
        | _ -> ("", ctxt'))
    | MacroApply (x, es) ->
        let f_x = Hashtbl.find_opt macro_hashtable_html x in
        let res_x = Option.map_default (fun f -> f) None f_x in
        (sprintf "%s" (match res_x with Some s -> s | None -> x), ctxt')
    | ExprStmt e ->
        ( sprintf "<li>%s%s%s.</li>" prepend_str
            E_Expr.(to_html ExprStmt e)
            append_str,
          ctxt' )
    | Assign (x, e) when is_special_expr e ->
        (sprintf "<li>%s.</li>" (expr_to_html e), ctxt')
    | Assign (x, e) ->
        let contents =
          sprintf "<i>%s</i> be %s%s" x E_Expr.(to_html Let e) append_str
        in
        ( (match ctxt' with
          | SameParagraph -> sprintf "%slet %s" prepend_str contents
          | _ ->
              sprintf "<li>%s%s %s.</li>" prepend_str
                (if prepend_str = "" then "Let" else "let")
                contents),
          ctxt' )
    | Return e when ctxt' = Table ->
        let html =
          match e with
          | E_Expr.Var _ ->
              "The result equals the input argument (no conversion)."
          | E_Expr.Val Val.Null -> "Return"
          | _ -> E_Expr.to_html E_Expr.Table e
        in
        (html, CNone)
    | Return e -> (
        match (ctxt, e) with
        | SameParagraph, E_Expr.Val Val.Null ->
            (sprintf "%sreturn%s" prepend_str append_str, CNone)
        | SameParagraph, _ ->
            ( sprintf "%sreturn %s%s" prepend_str (expr_to_html e) append_str,
              CNone )
        | _, E_Expr.Val Val.Null ->
            ( sprintf "<li>%s%s%s.</li>" prepend_str
                (if prepend_str = "" then "Return" else "return")
                append_str,
              ctxt' )
        | _, E_Expr.Var "Identifier" ->
            ( sprintf
                "<li>%s%s a String value containing the same sequence of \
                 characters as in the <i>Identifier</i>%s.</li>"
                prepend_str
                (if prepend_str = "" then "Return" else "return")
                append_str,
              ctxt' )
        | _ ->
            ( sprintf "<li>%s%s %s%s.</li>" prepend_str
                (if prepend_str = "" then "Return" else "return")
                (expr_to_html e) append_str,
              ctxt' ))
    | Throw e when ctxt' = Table -> (sprintf "Throw %s." (expr_to_html e), CNone)
    | Throw e -> (
        let e_html = expr_to_html e in
        match ctxt with
        | SameParagraph ->
            let str = sprintf "%sthrow %s" prepend_str e_html in
            ( (match append_str with
              | "" -> str
              | s -> sprintf "%s. %s" str append_str),
              CNone )
        | _ ->
            ( sprintf "<li>%s%s %s. %s</li>" prepend_str
                (if prepend_str = "" then "Throw" else "throw")
                e_html append_str,
              ctxt' ))
    | FieldAssign (e_o, f, e_v) ->
        let contents =
          sprintf "%s%s %s.%s to the value of %s. %s" prepend_str
            (if prepend_str = "" && not (ctxt' = SameParagraph) then "Set"
            else "set")
            (expr_to_html e_o) (expr_to_html f)
            E_Expr.(to_html Set e_v)
            append_str
        in
        ( (match ctxt' with
          | SameParagraph -> contents
          | _ -> "<li>" ^ contents ^ "</li>"),
          ctxt' )
    | FieldDelete (e, f) ->
        ( (match e with
          | E_Expr.Lookup (e', f') -> (
              match f' with
              | E_Expr.Val (Val.Str "JSProperties") ->
                  sprintf
                    "<li>%s%s the own property with name %s from %s. %s</li>"
                    prepend_str
                    (if prepend_str = "" then "Remove" else "remove")
                    (expr_to_html f) (expr_to_html e') append_str
              | _ ->
                  sprintf "<li>%s%s %s %s. %s<li>" prepend_str
                    (if prepend_str = "" then "Remove" else "remove")
                    (expr_to_html e) (expr_to_html f) append_str)
          | _ ->
              sprintf "<li>%s%s the binding for %s from %s. %s</li>" prepend_str
                (if prepend_str = "" then "Remove" else "remove")
                (expr_to_html f) (expr_to_html e) append_str),
          ctxt' )
    | Block stmts ->
        let htmls, ctxt_block =
          List.fold_left
            (fun (htmls, ctxt_arg) s ->
              let s_html, ctxt' = to_html ~ctxt:ctxt_arg s in
              let ctxt'' = match ctxt' with CNone -> ctxt | _ -> ctxt' in
              (htmls @ [ s_html ], ctxt''))
            ([], ctxt) stmts
        in
        (String.concat "" htmls, ctxt_block)
    | RepeatUntil (s, e, meta) ->
        let after_same, _ = parse_if_meta meta ctxt in
        ( sprintf "<li>Repeat%s<ol class=\"block\">%s</ol></li>" after_same
            (fst (to_html ~ctxt:ctxt' s)),
          ctxt' )
    | While (e, s) ->
        ( sprintf "<li>Repeat, while %s<ol class=\"block\">%s</ol></li>"
            (expr_to_html e)
            (fst (to_html ~ctxt:ctxt' s)),
          ctxt' )
    | ForEach (x, e, s, meta, var_meta) ->
        let var_prepend_str =
          match var_meta with Some (s, alt) when s = x -> alt | _ -> ""
        in
        let after_same, next_ctxt = parse_if_meta meta ctxt in
        ( sprintf "<li>For each %s %s in %s%s<ol class=\"block\">%s</ol></li>"
            var_prepend_str
            (expr_to_html (E_Expr.Var x))
            (expr_to_html e) after_same
            (fst (to_html ~ctxt:ctxt' s)),
          ctxt' )
    | If (e, s, None, _, _) when ctxt' = Table ->
        ( sprintf "The result is %s if the %s. "
            (fst (to_html ~ctxt:ctxt' s))
            E_Expr.(to_html Table e),
          ctxt' )
    | If (e, s1, Some s2, _, _) when ctxt' = Table ->
        ( sprintf "The result is %s if the %s; otherwise the result is %s."
            (fst (to_html ~ctxt:ctxt' s1))
            E_Expr.(to_html Table e)
            (fst (to_html ~ctxt:ctxt' s2)),
          ctxt' )
    | If (e, s, None, if_meta, _)
      when is_split_if if_meta && parse_if_split_meta if_meta = "" ->
        (* Ignore if_split_meta as it's expected to be empty, since this syntax is only supposed to appear at the middle of a block of statements *)
        (sprintf "%s" (fst (to_html s)), ctxt')
    | If (e, s1, None, if_meta, _) when is_split_if if_meta ->
        let split_html = parse_if_split_meta if_meta in
        ( sprintf
            (* Since the function contents are wrapped in <ol> and this "if statement"
               with "split-if" metadata only occurs as the first statement of the function block,
               we only need to use this simple template in the sprintf *)
            "</ol>%s<ol class=\"proc\">%s" split_html
            (fst (to_html s1)),
          ctxt' )
    | If (e, s, None, if_meta, _) when ctxt = SameParagraph ->
        let after_same, next_ctxt = parse_if_meta if_meta ctxt in
        ( sprintf "if %s, %s %s"
            E_Expr.(to_html IfGuard e)
            after_same
            (fst (to_html ~ctxt:SameParagraph s)),
          next_ctxt )
    | If (e, s, None, if_meta, _) when is_basic_and_not_call s && swappable s ->
        ( sprintf "<li>%s, if %s.</li>"
            (fst (to_html ~ctxt:SameParagraph s))
            E_Expr.(to_html IfGuard e),
          ctxt' )
    | If (e, s, None, if_meta, _) when is_basic_and_not_call s ->
        let after_same, next_ctxt = parse_if_meta if_meta ctxt in
        ( sprintf "<li>If %s, %s %s.</li>"
            E_Expr.(to_html IfGuard e)
            after_same
            (fst (to_html ~ctxt:SameParagraph s)),
          next_ctxt )
    | If (e, s, None, if_meta, _) ->
        let after_same, next_ctxt = parse_if_meta if_meta ctxt in
        ( sprintf "<li>If %s%s<ol class=\"block\">%s</ol></li>"
            E_Expr.(to_html IfGuard e)
            after_same
            (fst (to_html s)),
          next_ctxt )
    | If (e, s1, Some s2, if_meta, else_meta)
      when is_let_stmt s1 && is_let_stmt s2 ->
        let after_same_if, _ = parse_if_meta if_meta ctxt in
        let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
        ( sprintf "<li>If %s, %s then %s, %s%s %s.</li>"
            E_Expr.(to_html IfGuard e)
            after_same_if
            (fst (to_html ~ctxt:SameParagraph s1))
            (if replace_str <> "" then replace_str else "else")
            after_same_else
            (fst (to_html ~ctxt:SameParagraph s2)),
          ctxt' )
    | If (e, s1, Some s2, if_meta, else_meta)
      when is_basic_and_not_call s1 && is_basic_and_not_call s2 ->
        let after_same_if, _ = parse_if_meta if_meta ctxt in
        let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
        ( sprintf "<li>If %s, %s %s</li><li>%s%s %s</li>"
            E_Expr.(to_html IfGuard e)
            after_same_if
            (fst (to_html ~ctxt:SameParagraph s1))
            (if replace_str <> "" then replace_str else "Else")
            after_same_else
            (fst (to_html ~ctxt:SameParagraph s2)),
          next_ctxt )
    | If (e, s1, Some s2, if_meta, else_meta)
      when is_basic_and_not_call s2 && not (has_after_same_meta else_meta) ->
        let after_same_if, _ = parse_if_meta if_meta ctxt in
        let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
        ( sprintf "<li>If %s%s<ol class=\"block\">%s</ol></li><li>%s%s %s</li>"
            E_Expr.(to_html IfGuard e)
            after_same_if
            (fst (to_html s1))
            (if replace_str <> "" then replace_str else "Else")
            after_same_else
            (fst (to_html ~ctxt:SameParagraph s2)),
          next_ctxt )
    | If (e, s1, Some s2, if_meta, else_meta)
      when is_basic_and_not_call s1 && not (is_special_assign_expr s1) ->
        let after_same_if, _ = parse_if_meta if_meta ctxt in
        let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
        ( sprintf "<li>If %s, %s %s</li><li>%s%s<ol class=\"block\">%s</ol></li>"
            E_Expr.(to_html IfGuard e)
            after_same_if
            (fst (to_html ~ctxt:SameParagraph s1))
            (if replace_str <> "" then replace_str else "Else")
            after_same_else
            (fst (to_html s2)),
          next_ctxt )
    | If (e, s1, Some s2, if_meta, else_meta) ->
        let after_same_if, _ = parse_if_meta if_meta ctxt in
        let after_same_else, next_ctxt = parse_if_meta else_meta ctxt in
        ( sprintf
            "<li>If %s%s<ol class=\"block\">%s</ol></li><li>%s%s<ol \
             class=\"block\">%s</ol></li>"
            E_Expr.(to_html IfGuard e)
            after_same_if
            (fst (to_html s1))
            (if replace_str <> "" then replace_str else "Else")
            after_same_else
            (fst (to_html s2)),
          next_ctxt )
    | EIf (ifs, final_else) ->
        let e, s, m = List.hd ifs in
        let ifs' = List.tl ifs in
        let first_if_html =
          match is_basic_and_not_call s with
          | false ->
              sprintf "<li>If %s%s<ol class=\"block\">%s</ol></li>"
                E_Expr.(to_html IfGuard e)
                (fst (parse_if_meta m ctxt))
                (fst (to_html s))
          | true ->
              sprintf "<li>If %s, %s %s</li>"
                E_Expr.(to_html IfGuard e)
                (fst (parse_if_meta m ctxt))
                (fst (to_html ~ctxt:SameParagraph s))
        in
        let other_ifs_html =
          String.concat ""
            (List.map
               (fun (e, s, m) ->
                 match is_basic_and_not_call s with
                 | false ->
                     sprintf "<li>Else, if %s%s<ol class=\"block\">%s</ol></li>"
                       E_Expr.(to_html IfGuard e)
                       (fst (parse_if_meta m ctxt))
                       (fst (to_html s))
                 | true ->
                     sprintf "<li>Else, if %s, %s %s</li>"
                       E_Expr.(to_html IfGuard e)
                       (fst (parse_if_meta m ctxt))
                       (fst (to_html ~ctxt:SameParagraph s)))
               ifs')
        in
        let final_else_html =
          match final_else with
          | None -> ""
          | Some (s, m) ->
              sprintf "<li>Else%s<ol class=\"block\">%s</ol></li>"
                (fst (parse_if_meta m ctxt))
                (fst (to_html s))
        in
        (first_if_html ^ other_ifs_html ^ final_else_html, ctxt)
    | Switch (e, es_ss, sd, meta) ->
        let table_caption = meta in
        let contain_ifs (stmts : t list) : bool =
          List.exists (fun s -> match s with If _ -> true | _ -> false) stmts
        in
        let cell_data (s : t) =
          match s with
          | Block stmts when not (contain_ifs stmts) ->
              sprintf
                "<p>Apply the following steps:</p><ol class=\"proc\">%s</ol>"
                (fst (to_html s))
          | _ -> sprintf "%s" (fst (to_html ~ctxt:Table s))
        in
        let rows =
          List.map
            (fun (e, s) ->
              sprintf "<tr><td>%s</td><td>%s</td></tr>" (expr_to_html e)
                (cell_data s))
            es_ss
        in
        ( sprintf
            "<figure><figcaption>%s</figcaption><table \
             class=\"real-table\"><tbody><tr><th style=\"border-bottom: 1px \
             solid #000000; border-left: 1px solid #000000; border-top: 2px \
             solid #000000\">%s</th><th style=\"border-bottom: 1px solid \
             #000000; border-right: 1px solid #000000; border-top: 2px solid \
             #000000\">Result</th></tr>%s</tbody></table></figure>"
            table_caption
            E_Expr.(to_html Table e)
            (String.concat "" rows),
          Table )
    | Lambda _ -> ("", ctxt')
end

module E_Func = struct
  include E_Func

  let is_descendent_of (f1 : t) (f2 : t) : bool =
    let f1_metadata = get_metadata f1 in
    let f2_metadata = get_metadata f2 in
    match (f1_metadata, f2_metadata) with
    | Some m1, Some m2 ->
        let f1_sec_number = E_Func_Metadata.get_section_number m1 in
        let f2_sec_number = E_Func_Metadata.get_section_number m2 in
        let f2_sub_sec_number =
          String.sub f2_sec_number 0 (String.length f1_sec_number)
        in
        (* We don't want to have E_Func with the same section_number as descendents *)
        f1_sec_number <> f2_sec_number && f2_sub_sec_number = f1_sec_number
    | _ ->
        invalid_arg
          "Error in \"HTMLExtensions.is_descendent_of\": at least one of the \
           arguments doesn't have metadata."

  let to_html ?(inner_sections = "") (f : t) : string =
    let e_func_metadata = get_metadata f in
    match e_func_metadata with
    | None ->
        invalid_arg
          "Exception when executing to_html in a function without metadata."
    | Some meta -> (
        let params_meta = E_Func_Metadata.get_meta_params meta in
        HTMLHashtables.E_Expr.init_var_bindings params_meta;
        let metadata_pre = E_Func_Metadata.get_pre meta in
        let metadata_post = E_Func_Metadata.get_post meta in
        let metadata_sec_number = E_Func_Metadata.get_section_number meta in
        let metadata_sec_name = E_Func_Metadata.get_section_name meta in
        let pre_note =
          match metadata_pre with "" -> "" | pre -> "<p>" ^ pre ^ "</p>"
        in
        let post_note =
          match metadata_post with "" -> "" | post -> "<p>" ^ post ^ "</p>"
        in
        let header =
          match metadata_sec_name with
          | None -> ""
          | Some s ->
              let sec_name = match s with "" -> get_name f | _ -> s in
              let func_params =
                match get_params f with
                | [] -> ""
                | ps -> Printf.sprintf "(%s)" (String.concat ", " ps)
              in
              sprintf
                "<h1><span class=\"secnum\"><a href=\"#sec-%s\" title=\"link \
                 to this section\">%s</a></span>%s %s</h1>"
                metadata_sec_number metadata_sec_number sec_name func_params
        in
        let body, ctxt = E_Stmt.to_html (get_body f) in
        let body' =
          match ctxt with
          | E_Stmt.Table | _ -> sprintf "<ol class=\"proc\">%s</ol>" body
        in
        let section_contents =
          sprintf "%s%s%s%s" pre_note body' inner_sections post_note
        in
        match header with
        | "" -> section_contents
        | h ->
            sprintf "<section id=\"sec-%s\">%s%s</section>" metadata_sec_number
              header section_contents)
end

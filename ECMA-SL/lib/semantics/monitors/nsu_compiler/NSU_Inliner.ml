open Stmt
open Func
open Source
open NSU_CompilerConstants

(*Each monitor is independent of the other ones*)
exception Except of string

module M (SL : SecLevel.M) = struct
  let shadow_var_e (s : string) : Expr.t = Expr.Var (shadowvar s)

  let binopt_e (op : Operators.bopt) (e1 : Expr.t) (e2 : Expr.t) : Expr.t =
    Expr.BinOpt (op, e1, e2)

  let bottom_e () : Expr.t = Expr.Val (Val.Str (SL.str (SL.get_low ())))
  let shadow_fun_e () : Expr.t = Expr.Val (Val.Str _SHADOW_FUN_NAME_)
  let block_s (stmts : Stmt.t list) : Stmt.t = Stmt.Block stmts @@ no_region
  let lub_func () : Expr.t = Expr.Val (Val.Str _LUB_)
  let lubn_func () : Expr.t = Expr.Val (Val.Str _LUBN_)
  let parse_lvl_func () : Expr.t = Expr.Val (Val.Str _PARSE_LVL_)
  let leq_func () : Expr.t = Expr.Val (Val.Str _LEQ_)

  let c_expr (e : Expr.t) : Stmt.t * string =
    let xs = Expr.vars e in
    let x_lev = fresh_expr_lev () in
    let e =
      Expr.NOpt (Operators.ListExpr, List.map (fun x -> shadow_var_e x) xs)
    in
    (Stmt.AssignCall (x_lev, lubn_func (), [ e ]) @@ no_region, x_lev)

  let prepare_args (es : Expr.t list) : Expr.t list * Stmt.t list =
    let lst =
      List.map
        (fun e ->
          let s, x = c_expr e in
          ([ e; Expr.Var x ], s))
        es
    in
    let args, stmts = List.split lst in
    (List.concat args, stmts)

  (*
C_exp(e1) = stmt_1, x1_lev
C_exp(e2) = stmt_2, x2_lev
x_o, x_p, lub_1, leq_1 fresh

-------------------------------
C(x := e1[e2]) =
   stmt_1;
   stmt_2;
   x_o := e1;
   x_p := e2;
   x_o_lev := lubn(vars(x_o))
   p_shadow := shadow(x_p);
   x_p_l := x_o[p_shadow];
   lub_1 := lubn(pc, x1_lev, x2_lev);
   leq_1 := leq(lub_1, x_lev);
   if (leq_1) {
      lub_2 := lub(x_p_l, lub_1);
      x_lev := lub_2;
      x := x_o[x_p]
   } else {
       throw("IFlow Exception")
   }
*)
  let c_fieldlookup (pc : string) (x : string) (e_o : Expr.t) (e_f : Expr.t) :
      Stmt.t list =
    let stmt_1, x_1_lev = c_expr e_o in
    let stmt_2, x_2_lev = c_expr e_f in
    let x_o = fresh_obj () in
    let x_f = fresh_field () in
    let f_shadow = fresh_var () in
    let x_f_lev = fresh_field_lev () in
    let lub_1 = fresh_var_lev () in
    let lub_2 = fresh_var_lev () in
    let leq_1 = fresh_var () in
    let assigns =
      [
        stmt_1;
        stmt_2;
        Stmt.Assign (x_o, e_o) @@ no_region;
        Stmt.Assign (x_f, e_f) @@ no_region;
        Stmt.AssignCall
          (f_shadow, Expr.Val (Val.Str _SHADOW_PROP_VALUE_), [ Expr.Var x_f ])
        @@ no_region;
        Stmt.FieldLookup (x_f_lev, Expr.Var x_o, Expr.Var f_shadow) @@ no_region;
        Stmt.AssignCall
          ( lub_1,
            lubn_func (),
            [
              Expr.NOpt
                ( Operators.ListExpr,
                  [ Expr.Var x_1_lev; Expr.Var x_2_lev; Expr.Var pc ] );
            ] )
        @@ no_region;
        Stmt.AssignCall
          (leq_1, leq_func (), [ Expr.Var lub_1; Expr.Var (shadowvar x) ])
        @@ no_region;
        Stmt.If
          ( Expr.Var leq_1,
            Stmt.Block
              [
                Stmt.AssignCall
                  (lub_2, lub_func (), [ Expr.Var x_f_lev; Expr.Var lub_1 ])
                @@ no_region;
                Stmt.Assign (shadowvar x, Expr.Var lub_2) @@ no_region;
                FieldLookup (x, Expr.Var x_o, Expr.Var x_f) @@ no_region;
              ]
            @@ no_region,
            Some
              (Stmt.Block
                 [
                   Stmt.Exception "MONITOR EXCEPTION -> Illegal Field Lookup"
                   @@ no_region;
                 ]
              @@ no_region) )
        @@ no_region;
      ]
    in

    assigns

  (*
pc' fresh
C_exp(e) = (stmt_e, x_e_lev)
C(s1, pc') = stmts_1
C(s2, pc') = stmts_2
--------------------
C(if(e){s1} else {s2}) =
  stmt_e;
  pc' := lub(pc, e_l);
  if(e){stmts_1} else {stmts_2}
*)
  let c_if (pc : string) (pc' : string) (e : Expr.t) (stmts_1 : Stmt.t list)
      (stmts_2 : Stmt.t list) : Stmt.t list =
    let stmt_e, x_e_lev = c_expr e in
    let lub_1 = fresh_var_lev () in
    let if_stmt =
      match stmts_2 with
      | [] -> Stmt.If (e, block_s stmts_1, None)
      | _ -> Stmt.If (e, block_s stmts_1, Some (block_s stmts_2))
    in
    [
      stmt_e;
      Stmt.AssignCall (lub_1, lub_func (), [ Expr.Var pc; Expr.Var x_e_lev ])
      @@ no_region;
      Stmt.Assign (pc', Expr.Var lub_1) @@ no_region;
      if_stmt @@ no_region;
    ]

  (*
  C_exp(e)= (stmt_e, x_e_lev)
  ret_lev fresh
  --------------
  C(return e)=
    stmt_e;
    ret_lev := lub(pc, x_e_lev);
    return (e, ret_lev)
*)
  let c_return (pc : string) (e : Expr.t) : Stmt.t list =
    let stmt_e, x_e_lev = c_expr e in
    let lub_1 = fresh_var_lev () in
    [
      stmt_e;
      Stmt.AssignCall (lub_1, lub_func (), [ Expr.Var x_e_lev; Expr.Var pc ])
      @@ no_region;
      Stmt.Return (Expr.NOpt (Operators.TupleExpr, [ e; Expr.Var lub_1 ]))
      @@ no_region;
    ]

  (*
  pc' fresh
  C_exp(e)= (stmt_e, x_e_lev)
  C(s, pc') = stmts
  ---------------------
  C(while(e){s1})=
    stmt_e;
    pc' := lub(pc, x_e_lev);
    while(e){stmts}
*)
  let c_while (pc : string) (pc' : string) (e : Expr.t) (stmts : Stmt.t list) :
      Stmt.t list =
    let stmt_e, x_e_lev = c_expr e in
    let while_stmt =
      [ Stmt.While (e, Stmt.Block stmts @@ no_region) @@ no_region ]
    in
    [
      stmt_e;
      Stmt.AssignCall (pc', lub_func (), [ Expr.Var pc; Expr.Var x_e_lev ])
      @@ no_region;
    ]
    @ while_stmt

  (*
  C_exp(e)= (stmt_e, x_e_lev)
  leq_1 fresh
  -------------
  C(x := e)=
    leq_1 := leq(pc; x_lev);
    if(leq_1){
      stmt_e;
      x_lev := lub(pc, x_e_lev);
      x := e
    } else {
      throw("IFlow Exception")
    }
*)
  let c_assign (pc : string) (x : string) (e : Expr.t) : Stmt.t list =
    let stmt_e, x_e_lev = c_expr e in
    let x_shadow = shadow_var_e x in
    let leq_1 = fresh_var () in
    let st1 =
      Stmt.Block
        [
          stmt_e;
          Stmt.AssignCall
            (shadowvar x, lub_func (), [ Expr.Var pc; Expr.Var x_e_lev ])
          @@ no_region;
          Stmt.Assign (x, e) @@ no_region;
        ]
    in
    [
      Stmt.AssignCall (leq_1, leq_func (), [ Expr.Var pc; x_shadow ])
      @@ no_region;
      Stmt.If
        ( Expr.Var leq_1,
          st1 @@ no_region,
          Some
            (Stmt.Block
               [
                 Stmt.Exception "MONITOR EXCEPTION -> Illegal Assignment"
                 @@ no_region;
               ]
            @@ no_region) )
      @@ no_region;
    ]

  (*
  res, leq_1 fresh
  prepare_args(args) := (args', stmt_args)
  ----------------
  C(x := f(args))=
    stmt_args;
    leq_1 := leq(pc, x_lev);
    if(leq_1){
      res := f(args', pc);
      x := first(res);
      x_lev := second(res);
    } else {
      throw("MONITOR BLOCK - pc bigger than x")
    }
*)
  let c_assigncall (pc : string) (x : string) (f : Expr.t) (args : Expr.t list)
      : Stmt.t list =
    let new_args, stmt_args = prepare_args args in
    let res = fresh_var () in
    let leq_1 = fresh_var () in
    let st1 =
      [
        Stmt.AssignCall (res, f, new_args @ [ Expr.Var pc ]) @@ no_region;
        Stmt.Assign (x, Expr.UnOpt (Operators.First, Expr.Var res)) @@ no_region;
        Stmt.Assign (shadowvar x, Expr.UnOpt (Operators.Second, Expr.Var res))
        @@ no_region;
      ]
    in
    stmt_args
    @ [
        Stmt.AssignCall (leq_1, leq_func (), [ Expr.Var pc; shadow_var_e x ])
        @@ no_region;
        Stmt.If
          ( Expr.Var leq_1,
            Stmt.Block st1 @@ no_region,
            Some
              (Stmt.Block
                 [
                   Stmt.Exception
                     "MONITOR EXCEPTION -> Pc bigger than x in AssignCall"
                   @@ no_region;
                 ]
              @@ no_region) )
        @@ no_region;
      ]

  (*
  C(e)= (stmt_e, x_e_lev)
  leq_1 fresh
  -----------
  C(print(e))=
    stmt_e;
    leq_1 =  lec(pc, x_e_lev);
    if(leq_1){
      print(e)
    } else {
     throw("Illegal Print")
    }

*)
  let c_print (pc : string) (e : Expr.t) : Stmt.t list =
    let stmt_e, x_e_lev = c_expr e in
    let leq_1 = fresh_var () in
    [
      stmt_e;
      Stmt.AssignCall (leq_1, leq_func (), [ Expr.Var pc; Expr.Var x_e_lev ])
      @@ no_region;
      Stmt.If
        ( Expr.Var leq_1,
          Stmt.Block [ Stmt.Print e @@ no_region ] @@ no_region,
          Some
            (Stmt.Block
               [
                 Stmt.Exception "MONITOR EXCEPTION -> Illegal Print"
                 @@ no_region;
               ]
            @@ no_region) )
      @@ no_region;
    ]

  (*
------------
C(x:={})=
  x:= {};
  x["struct_lev"] := pc;
  x["object_lev"] := pc
*)
  let c_assignnewobj (pc : string) (x : string) : Stmt.t list =
    (*TODO - NSU*)
    let x_lev = shadowvar x in
    [
      Stmt.AssignNewObj x @@ no_region;
      Stmt.FieldAssign
        (Expr.Var x, Expr.Val (Val.Str _OBJ_STRUCT_LEV_PROP_), Expr.Var pc)
      @@ no_region;
      Stmt.FieldAssign
        (Expr.Var x, Expr.Val (Val.Str _OBJ_VALUE_LEV_PROP_), Expr.Var pc)
      @@ no_region;
      Stmt.Assign (x_lev, Expr.Var pc) @@ no_region;
    ]

  (*
C(e_o) = (stmt_x_o, x_o_lev)
C(e_f) = (stmt_x_f, x_f_lev)
C(e_v) = (stmt_x_v, x_v_lev)
prop_val_lev_name prop_val_lev fresh
prop_exists_lev_name fresh
struct_lvl_name, struct_lvl, leq_1, lub_1 fresh
-------------------------------------
C(e_o[e_f]:= e_v)=
  stmt_x_o;
  stmt_x_f;
  stmt_x_v;
  x_o := e_o;
  x_f := e_f;
  x_v := e_v;
  ctx := lubn(x_o_lev, x_f_lev, pc);
  prop_val_lev_name := propVal(x_f);
  prop_val_lev := x_o[prop_val_lev_name];
  if(prop_val_lev != null){
    leq_1 := leq(ctx, prop_val_lev);
    if(leq_1){
      lub_1 := lub(ctx, x_v_lev)
      x_o[prop_val_lev_name] := lub_1;
      x_o[x_f] := x_v;
    } else {
      throw("Illegal Field Assign")
    }
  } else{
    struct_lev_name := structLevName();
    struct_lev := x_o[struct_lev_name];
    leq_1 := leq(ctx, struct_lev);
    if(leq_1){
    prop_exists_lev_name := propExists(x_f);
    lub_1 := lub(ctx, x_v_lev);
    x_o[prop_val_lev_name] := lub_1;
    x_o[prop_exists_lev_name] := ctx;
    x_o[x_f] := x_v;
    } else{
      throw("Illegal Field Creation")
    }
  }
*)
  let c_fieldassign (pc : string) (e_o : Expr.t) (e_f : Expr.t) (e_v : Expr.t) :
      Stmt.t list =
    let stmt_x_o, x_o_lev = c_expr e_o in
    let stmt_x_f, x_f_lev = c_expr e_f in
    let stmt_x_v, x_v_lev = c_expr e_v in
    let x_o = e_o in
    let x_f = e_f in
    let x_v = e_v in
    let ctx = fresh_var_lev () in
    let leq_1 = fresh_var () in
    let lub_1 = fresh_var_lev () in
    let prop_val_lev_name = fresh_var () in
    let prop_val_lev = fresh_field_lev () in
    let prop_exists_lev_name = fresh_var () in
    let struct_lev = fresh_field_lev () in
    [
      stmt_x_o;
      stmt_x_f;
      stmt_x_v;
      Stmt.AssignCall
        ( ctx,
          lubn_func (),
          [
            Expr.NOpt
              ( Operators.ListExpr,
                [ Expr.Var x_o_lev; Expr.Var x_f_lev; Expr.Var pc ] );
          ] )
      @@ no_region;
      Stmt.AssignCall
        (prop_val_lev_name, Expr.Val (Val.Str _SHADOW_PROP_VALUE_), [ x_f ])
      @@ no_region;
      Stmt.FieldLookup (prop_val_lev, x_o, Expr.Var prop_val_lev_name)
      @@ no_region;
      Stmt.If
        ( binopt_e Operators.Eq (Expr.Var prop_val_lev)
            (Expr.Val (Val.Symbol "undefined")),
          Stmt.Block
            [
              Stmt.FieldLookup
                (struct_lev, x_o, Expr.Val (Val.Str _OBJ_STRUCT_LEV_PROP_))
              @@ no_region;
              Stmt.AssignCall
                (leq_1, leq_func (), [ Expr.Var ctx; Expr.Var struct_lev ])
              @@ no_region;
              Stmt.If
                ( Expr.Var leq_1,
                  Stmt.Block
                    [
                      Stmt.AssignCall
                        ( prop_exists_lev_name,
                          Expr.Val (Val.Str _SHADOW_PROP_EXISTS_),
                          [ x_f ] )
                      @@ no_region;
                      Stmt.AssignCall
                        (lub_1, lub_func (), [ Expr.Var ctx; Expr.Var x_v_lev ])
                      @@ no_region;
                      Stmt.FieldAssign
                        (x_o, Expr.Var prop_val_lev_name, Expr.Var lub_1)
                      @@ no_region;
                      Stmt.FieldAssign
                        (x_o, Expr.Var prop_exists_lev_name, Expr.Var ctx)
                      @@ no_region;
                      Stmt.FieldAssign (x_o, x_f, x_v) @@ no_region;
                    ]
                  @@ no_region,
                  Some
                    (Stmt.Block
                       [
                         Stmt.Exception
                           "MONITOR EXCEPTION -> Illegal Field Assign"
                         @@ no_region;
                       ]
                    @@ no_region) )
              @@ no_region;
            ]
          @@ no_region,
          Some
            (Stmt.Block
               [
                 Stmt.AssignCall
                   (leq_1, leq_func (), [ Expr.Var ctx; Expr.Var prop_val_lev ])
                 @@ no_region;
                 Stmt.If
                   ( Expr.Var leq_1,
                     Stmt.Block
                       [
                         Stmt.AssignCall
                           ( lub_1,
                             lubn_func (),
                             [
                               Expr.NOpt
                                 ( Operators.ListExpr,
                                   [ Expr.Var ctx; Expr.Var x_v_lev ] );
                             ] )
                         @@ no_region;
                         Stmt.FieldAssign
                           (x_o, Expr.Var prop_val_lev_name, Expr.Var lub_1)
                         @@ no_region;
                         Stmt.FieldAssign (x_o, x_f, x_v) @@ no_region;
                       ]
                     @@ no_region,
                     Some
                       (Stmt.Block
                          [
                            Stmt.Exception
                              "MONITOR EXCEPTION -> Illegal Field Assign"
                            @@ no_region;
                          ]
                       @@ no_region) )
                 @@ no_region;
               ]
            @@ no_region) )
      @@ no_region;
    ]

  (*
C(e_o) = (stmt_x_o, x_o_lev)
C(e_f) = (stmt_x_f, x_f_lev)
prop_exists_lev, prop_exists_lev_name, leq_1, ctx fresh
------------------------------------------
C(delete(e_o[e_f]))=
  stmt_x_o;
  stmt_x_f;
  x_o := e_o;
  x_f := e_f;
  ctx=lubn(x_o_lev, x_f_lev, pc);
  prop_exists_lev_name := propExists(x_f);
  prop_exists_lev := x_o[prop_exists_lev_name];
  if(prop_exists_lev == null){
    leq_1 := leq(ctx, prop_exists_lev);
    if(leq_1){
      delete(x_o[x_f])
    } else{
      throw("Illegal Field Delete")
    }
  } else {
    throw("Internal Error")
  }
*)
  let c_fielddelete (pc : string) (e_o : Expr.t) (e_f : Expr.t) : Stmt.t list =
    let stmt_x_o, x_o_lev = c_expr e_o in
    let stmt_x_f, x_f_lev = c_expr e_f in
    let x_o = e_o in
    let x_f = e_f in
    let ctx = fresh_var () in
    let leq_1 = fresh_var () in
    let prop_exists_lev_name = fresh_var () in
    let prop_exists_lev = fresh_field_lev () in
    [
      stmt_x_o;
      stmt_x_f;
      Stmt.AssignCall
        ( ctx,
          lubn_func (),
          [
            Expr.NOpt
              ( Operators.ListExpr,
                [ Expr.Var x_o_lev; Expr.Var x_f_lev; Expr.Var pc ] );
          ] )
      @@ no_region;
      Stmt.AssignCall
        (prop_exists_lev_name, Expr.Val (Val.Str _SHADOW_PROP_EXISTS_), [ x_f ])
      @@ no_region;
      Stmt.FieldLookup (prop_exists_lev, x_o, Expr.Var prop_exists_lev_name)
      @@ no_region;
      Stmt.If
        ( binopt_e Operators.Eq (Expr.Var prop_exists_lev)
            (Expr.Val (Val.Symbol "undefined")),
          Stmt.Block [ Stmt.Exception "Internal Error" @@ no_region ]
          @@ no_region,
          Some
            (Stmt.Block
               [
                 Stmt.AssignCall
                   ( leq_1,
                     leq_func (),
                     [ Expr.Var ctx; Expr.Var prop_exists_lev ] )
                 @@ no_region;
                 Stmt.If
                   ( Expr.Var leq_1,
                     Stmt.Block [ Stmt.FieldDelete (x_o, x_f) @@ no_region ]
                     @@ no_region,
                     Some
                       (Stmt.Block
                          [
                            Stmt.Exception
                              "MONITOR EXCEPTION -> Illegal Field Delete"
                            @@ no_region;
                          ]
                       @@ no_region) )
                 @@ no_region;
               ]
            @@ no_region) )
      @@ no_region;
    ]

  (*
  C(e_o)= (stmt_x_o, x_o_lev)
  C(e_f)= (stmt_x_f, x_f_lev)
  struct_lev, leq_1, ctx  fresh
  prop_exists_lev, prop_exists_lev_name fresh
  ----------------------
  C(x := e_f in(e_o)) =
  x_o := e_o;
  x_f := e_f;
  ctx := lubn(x_o_lev, x_f_lev, pc);
  struct_lev := x_o[struct_lev_name];
  prop_exists_lev_name := shadowPropExists(x_f);
  prop_exists_lev := x_o[prop_exists_lev_name];
  if(leq(ctx, x_lev)){
    if(prop_exists_lev == null){
      x_lev := lub(ctx, struct_lev);
    }else {
      x_lev := lub(prop_exists_lev, ctx)
    }
  }else{
      throw("Illegal Assignment")
    }
  }
  x := e_f in(x_o);
*)
  let c_assinginobjcheck (pc : string) (x : string) (e_f : Expr.t)
      (e_o : Expr.t) : Stmt.t list =
    let stmt_x_o, x_o_lev = c_expr e_o in
    let stmt_x_f, x_f_lev = c_expr e_f in
    let x_o = e_o in
    let x_f = e_f in
    let ctx = fresh_var () in
    let leq_1 = fresh_var () in
    let prop_exists_lev_name = fresh_var () in
    let prop_exists_lev = fresh_field_lev () in
    let struct_lev = fresh_field_lev () in
    [
      stmt_x_o;
      stmt_x_f;
      Stmt.AssignCall
        ( ctx,
          lubn_func (),
          [
            Expr.NOpt
              ( Operators.ListExpr,
                [ Expr.Var x_o_lev; Expr.Var x_f_lev; Expr.Var pc ] );
          ] )
      @@ no_region;
      Stmt.AssignCall
        (prop_exists_lev_name, Expr.Val (Val.Str _SHADOW_PROP_EXISTS_), [ x_f ])
      @@ no_region;
      Stmt.FieldLookup (prop_exists_lev, x_o, Expr.Var prop_exists_lev_name)
      @@ no_region;
      Stmt.FieldLookup
        (struct_lev, x_o, Expr.Val (Val.Str _OBJ_STRUCT_LEV_PROP_))
      @@ no_region;
      Stmt.AssignCall (leq_1, leq_func (), [ Expr.Var ctx; shadow_var_e x ])
      @@ no_region;
      Stmt.If
        ( Expr.Var leq_1,
          Stmt.Block
            [
              Stmt.If
                ( binopt_e Operators.Eq (Expr.Var prop_exists_lev)
                    (Expr.Val (Val.Symbol "undefined")),
                  Stmt.Block
                    [
                      Stmt.AssignCall
                        ( shadowvar x,
                          lub_func (),
                          [ Expr.Var ctx; Expr.Var struct_lev ] )
                      @@ no_region;
                    ]
                  @@ no_region,
                  Some
                    (Stmt.Block
                       [
                         Stmt.AssignCall
                           ( shadowvar x,
                             lub_func (),
                             [ Expr.Var ctx; Expr.Var prop_exists_lev ] )
                         @@ no_region;
                       ]
                    @@ no_region) )
              @@ no_region;
            ]
          @@ no_region,
          Some
            (Stmt.Block
               [
                 Stmt.Exception "MONITOR EXCEPTION -> Illegal Assignment"
                 @@ no_region;
               ]
            @@ no_region) )
      @@ no_region;
      Stmt.AssignInObjCheck (x, x_f, x_o) @@ no_region;
    ]

  (*
  x_shadow = shadow(x)
  leq_1 lub_1 fresh
  __________________________
  C(ret := upgVar(x, lev_str))=
    level := parse_lvl(lev_str);
    leq_1 := leq(pc, x_shadow);
    if (leq_1){
      x_shadow := lub(level, pc)
    } else {
      throw "MONITOR EXCEPTION -> Illegal UpgVarLab"
    }


*)
  let c_upgVar (pc : string) (ret : string) (x_name : string) (lev_str : string)
      : Stmt.t list =
    let x_shadow = shadowvar x_name in
    let level = fresh_var_lev () in
    let leq_1 = fresh_var () in
    [
      Stmt.AssignCall (level, parse_lvl_func (), [ Expr.Val (Val.Str lev_str) ])
      @@ no_region;
      Stmt.AssignCall (leq_1, leq_func (), [ Expr.Var pc; shadow_var_e x_name ])
      @@ no_region;
      Stmt.If
        ( Expr.Var leq_1,
          Stmt.Block
            [
              Stmt.AssignCall
                (x_shadow, lub_func (), [ Expr.Var level; Expr.Var pc ])
              @@ no_region;
            ]
           @@ no_region,
          Some
            (Stmt.Block
               [ Stmt.Exception "MONITOR EXCEPTION -> Illegal UpgVarLab" @@ no_region] @@ no_region) ) @@ no_region;
    ]
  (*(*
      C(e_o) = (stmt_x_o, x_o_lev)
      C(e_f) = (stmt_x_f, x_f_lev)
      x_shadow = shadow(x)
      leq_1 lub_1 lev_ctx leq_2 prop_2 fresh
      __________________________
      C( ret := upgPropExists (e_o, e_f, lev_str)) =
      stmt_x_o;
      stmt_x_f;
      x_o := e_o;
      x_f := e_f;
      lev := parse_lvl(lev_str);
      leq_1 := leq(lev_str_lev, pc);
      if(! leq_1){
        throw "upgPropExists - Levels must be literals"
      };
      lev_ctx := lubn(x_o_lev, x_f_lev, pc);
      prop_2 := shadowPropExists(x_f);
      lev_2 := x_o[prop_2];
      leq_2 := leq(lev_ctx, lev_2);
      if(leq_2){
        lev_3 := lub(lev_ctx, lev);
        x_o[prop_2] := lev_3
      } else{
        throw "MONITOR EXCEPTION -> Illegal P_Exists Upgrade"
      }
    }

    *)

    let c_upgPropExists (pc : string) (ret : string) (e_o : Expr.t) (e_f : Expr.t) (lev_str : string) : Stmt.t list =
      let (stmt_x_o, x_o_lev) = c_expr e_o in
      let (stmt_x_f, x_f_lev) = c_expr e_f in
      let x_o = e_o in
      let x_f = e_f in
      let lev_ctx = fresh_var_lev () in
      let lev = fresh_var_lev () in
      let lev_2 = fresh_var_lev () in
      let lev_3 = fresh_var_lev () in
      let leq_1 = fresh_var () in
      let leq_2 = fresh_var () in
      let prop_2 = fresh_var () in
      [
        stmt_x_o;
        stmt_x_f;
        Stmt.AssignCall (lev, parse_lvl_func (), [Expr.Val (Val.Str lev_str)]);
        Stmt.AssignCall (leq_1, leq_func (), [Expr.Val (Val.Str lev_str); Expr.Var pc]); (* uma vez estando sempre a ler strings isto nao e preciso*)
        Stmt.If (Expr.Var leq_1,
          Stmt.Block [
            Stmt.Exception "upgPropExists - Levels must be literals"
          ], None);
        Stmt.AssignCall (lev_ctx, lubn_func (), [Expr.Var x_o_lev; Expr.Var x_f_lev; Expr.Var pc]);
        Stmt.AssignCall (prop_2, Expr.Val (Val.Str _SHADOW_PROP_EXISTS_), [x_f]);
        Stmt.FieldLookup (lev_2, x_o, Expr.Var prop_2);
        Stmt.AssignCall (leq_2, leq_func (), [Expr.Var lev_ctx; Expr.Var lev_2]);
        Stmt.If (Expr.Var leq_2,
          Stmt.Block [
            Stmt.AssignCall (lev_3, lub_func (), [Expr.Var lev_ctx; Expr.Var lev]);
            Stmt.FieldAssign(x_o, Expr.Var prop_2, Expr.Var lev_3)
          ], Some (Stmt.Block [
            Stmt.Exception "MONITOR EXCEPTION -> Illegal P_Exists Upgrade"
          ]))
      ]

    (*C(lev_str) = (stmt_lev_str, lev_str_lev)
      C(e_o) = (stmt_x_o, x_o_lev)
      C(e_f) = (stmt_x_f, x_f_lev)
      x_shadow = shadow(x)
      leq_1 lub_1 lev_ctx leq_2 prop_2 fresh
      -------------------------------------
      C(ret := upgPropVal (e_o, e_f, lev_str)) =
      stmt_x_o;
      stmt_x_f;
      x_o := e_o;
      x_f := e_f;
      lev := parse_lvl(lev_str);
      leq_1 := leq(lev_str_lev, pc);
      if(! leq_1){
        throw "upgPropExists - Levels must be literals"
      };
      lev_ctx := lubn(x_o_lev, x_f_lev, pc);
      prop_2 := shadowPropVal(prop);
      lev_2 := obj[prop_2];
      leq_2 := leq(lev_ctx, lev_2);
      if(leq_2){
        lev_3 := lub(lev_ctx, lev);
        obj[prop_2] := lev_3
      } else{
        throw "MONITOR EXCEPTION -> Illegal P_Val Upgrade"
      }
    };*)

    let c_upgPropVal (pc : string) (ret : string) (e_o : Expr.t) (e_f : Expr.t) (lev_str : string) : Stmt.t list =
      let (stmt_x_o, x_o_lev) = c_expr e_o in
      let (stmt_x_f, x_f_lev) = c_expr e_f in
      let x_o = e_o in
      let x_f = e_f in
      let lev_ctx = fresh_var_lev () in
      let lev = fresh_var_lev () in
      let lev_2 = fresh_var_lev () in
      let lev_3 = fresh_var_lev () in
      let leq_1 = fresh_var () in
      let leq_2 = fresh_var () in
      let prop_2 = fresh_var () in
      [
        stmt_x_o;
        stmt_x_f;
        Stmt.AssignCall (lev, parse_lvl_func (), [Expr.Val (Val.Str lev_str)]);
        Stmt.AssignCall (leq_1, leq_func (), [Expr.Val (Val.Str lev_str); Expr.Var pc]); (* uma vez estando sempre a ler strings isto nao e preciso*)
        Stmt.If (Expr.Var leq_1,
          Stmt.Block [
            Stmt.Exception "upgPropExists - Levels must be literals"
          ], None);
        Stmt.AssignCall (lev_ctx, lubn_func (), [Expr.Var x_o_lev; Expr.Var x_f_lev; Expr.Var pc]);
        Stmt.AssignCall (prop_2, Expr.Val (Val.Str _SHADOW_PROP_VALUE_), [x_f]);
        Stmt.FieldLookup (lev_2, x_o, Expr.Var prop_2);
        Stmt.AssignCall (leq_2, leq_func (), [Expr.Var lev_ctx; Expr.Var lev_2]);
        Stmt.If (Expr.Var leq_2,
          Stmt.Block [
            Stmt.AssignCall (lev_3, lub_func (), [Expr.Var lev_ctx; Expr.Var lev]);
            Stmt.FieldAssign(x_o, Expr.Var prop_2, Expr.Var lev_3)
          ], Some (Stmt.Block [
            Stmt.Exception "MONITOR EXCEPTION -> Illegal P_Val Upgrade"
          ]))
      ]

    (*
      C(e_o) = (stmt_x_o, x_o_lev)
      leq_1  lvl_ctx fresh
      ----------------------
      C( upgStruct (e_o, lev_str))=
      stmt_x_o;
      x_o := e_o;
      lev := parse_lvl(lev_str);
      leq_1 := leq(lev_str_lev, pc);
      if(! leq_1){
        throw "upgStruct - Levels must be literals"
      };
      lev_ctx := lub(x_o_lev, pc);
      lev_2 := x_o["structLev"];
      leq_2 := leq(lev_ctx, lev_2);
      if(leq_2){
        lev_3 := lub(lev_ctx, lev);
        x_o["structLev"] := lev_3
      } else{
        throw "MONITOR EXCEPTION -> Illegal P_Val Upgrade"
      }

    *)

    let c_upgStruct (pc : string) (ret : string) (e_o : Expr.t) (lev_str : string) : Stmt.t list =
      let (stmt_x_o, x_o_lev) = c_expr e_o in
      let x_o = e_o in
      let lev_ctx = fresh_var_lev () in
      let lev = fresh_var_lev () in
      let lev_2 = fresh_var_lev () in
      let lev_3 = fresh_var_lev () in
      let leq_1 = fresh_var () in
      let leq_2 = fresh_var () in
      [
        stmt_x_o;
        Stmt.AssignCall (lev, parse_lvl_func (), [Expr.Val (Val.Str lev_str)]);
        Stmt.AssignCall (leq_1, leq_func (), [Expr.Val (Val.Str lev_str); Expr.Var pc]); (* uma vez estando sempre a ler strings isto nao e preciso*)
        Stmt.If (Expr.Var leq_1,
          Stmt.Block [
            Stmt.Exception "upgPropExists - Levels must be literals"
          ], None);
        Stmt.AssignCall (lev_ctx, lub_func (), [Expr.Var x_o_lev; Expr.Var pc]);
        Stmt.FieldLookup (lev_2, x_o, Expr.Val (Val.Str _OBJ_STRUCT_LEV_PROP_));
        Stmt.AssignCall (leq_2, leq_func (), [Expr.Var lev_ctx; Expr.Var lev_2]);
        Stmt.If (Expr.Var leq_2,
          Stmt.Block [
            Stmt.AssignCall (lev_3, lub_func (), [Expr.Var lev_ctx; Expr.Var lev]);
            Stmt.FieldAssign(x_o, Expr.Val (Val.Str _OBJ_STRUCT_LEV_PROP_), Expr.Var lev_3)
          ], Some (Stmt.Block [
            Stmt.Exception "MONITOR EXCEPTION -> Illegal P_Val Upgrade"
          ]))

      ]

      (*
      C(e_o) = (stmt_x_o, x_o_lev)
      leq_1  lvl_ctx fresh
      ----------------------
      C( upgObject (e_o, lev_str))=
      stmt_x_o;
      x_o := e_o;
      lev := parse_lvl(lev_str);
      leq_1 := leq(lev_str_lev, pc);
      if(! leq_1){
        throw "upgObject - Levels must be literals"
      };
      lev_ctx := lub(x_o_lev, pc);
      lev_2 := x_o["objLev"];
      leq_2 := leq(lev_ctx, lev_2);
      if(leq_2){
        lev_3 := lub(lev_ctx, lev);
        x_o["objLev"] := lev_3
      } else{
        throw "MONITOR EXCEPTION -> Illegal P_Val Upgrade"
      }

    *)

    let c_upgStruct (pc : string) (ret : string) (e_o : Expr.t) (lev_str : string) : Stmt.t list =
      let (stmt_x_o, x_o_lev) = c_expr e_o in
      let x_o = e_o in
      let lev_ctx = fresh_var_lev () in
      let lev = fresh_var_lev () in
      let lev_2 = fresh_var_lev () in
      let lev_3 = fresh_var_lev () in
      let leq_1 = fresh_var () in
      let leq_2 = fresh_var () in
      [
        stmt_x_o;
        Stmt.AssignCall (lev, parse_lvl_func (), [Expr.Val (Val.Str lev_str)]);
        Stmt.AssignCall (leq_1, leq_func (), [Expr.Val (Val.Str lev_str); Expr.Var pc]); (* uma vez estando sempre a ler strings isto nao e preciso*)
        Stmt.If (Expr.Var leq_1,
          Stmt.Block [
            Stmt.Exception "upgPropExists - Levels must be literals"
          ], None);
        Stmt.AssignCall (lev_ctx, lub_func (), [Expr.Var x_o_lev; Expr.Var pc]);
        Stmt.FieldLookup (lev_2, x_o, Expr.Val (Val.Str _OBJ_STRUCT_LEV_PROP_));
        Stmt.AssignCall (leq_2, leq_func (), [Expr.Var lev_ctx; Expr.Var lev_2]);
        Stmt.If (Expr.Var leq_2,
          Stmt.Block [
            Stmt.AssignCall (lev_3, lub_func (), [Expr.Var lev_ctx; Expr.Var lev]);
            Stmt.FieldAssign(x_o, Expr.Val (Val.Str _OBJ_STRUCT_LEV_PROP_), Expr.Var lev_3)
          ], Some (Stmt.Block [
            Stmt.Exception "MONITOR EXCEPTION -> Illegal P_Val Upgrade"
          ]))

      ]
  *)

  let rec compile (pc : string) (stmt : Stmt.t) : Stmt.t list =
    let compile_o pc = Option.map (compile pc) in

    match stmt.it with
    | Block lst -> c_block pc lst 
    | Skip -> [ Stmt.Skip @@ stmt.at ]
    | Assign (x, e) -> c_assign pc x e
    | Return e -> c_return pc e
    | If (e, s1, s2) ->
        let pc' = fresh_var () in
        let stmts_1 = compile pc' s1 in
        let stmts_2 = Option.default [] (compile_o pc' s2) in
        c_if pc pc' e stmts_1 stmts_2
    | While (e, s) ->
        let pc' = fresh_var () in
        let stmts = compile pc' s in
        c_while pc pc' e stmts
    | AssignCall
        ( ret,
          Expr.Val (Val.Str f),
          [ Expr.Val (Val.Str x_name); Expr.Val (Val.Str lev_str) ] )
      when f = _upgVar_ ->
        c_upgVar pc ret x_name lev_str
    | AssignCall (x, f, args) -> c_assigncall pc x f args
    | Print e -> c_print pc e
    | AssignInObjCheck (st, e_f, e_o) -> c_assinginobjcheck pc st e_f e_o
    | AssignNewObj x -> c_assignnewobj pc x
    | FieldLookup (x, e_o, e_f) -> c_fieldlookup pc x e_o e_f
    | FieldAssign (e_o, e_f, e_v) -> c_fieldassign pc e_o e_f e_v
    | FieldDelete (e_o, e_f) -> c_fielddelete pc e_o e_f
    | AssignObjToList (st, e) -> [ Stmt.AssignObjToList (st, e) @@ stmt.at ]
    | AssignObjFields (st, e) -> [ Stmt.AssignObjFields (st, e) @@ stmt.at ]
    | _ -> raise (Except ("Unknown Op -> " ^ Stmt.str stmt))
  (*ERROR*)

  and c_block (pc : string) (lst : Stmt.t list) : Stmt.t list =
    let res : Stmt.t list =
      List.fold_left (fun ac stmt -> ac @ compile pc stmt) [] lst
    in
    res

  let translist (pc : string) (_stmts : Stmt.t) : Stmt.t list =
    match _stmts.it with
    | Block stmts -> List.fold_left (fun ac s -> ac @ compile pc s) [] stmts
    | _ -> []

  let compile_functions (prog : Prog.t) : Prog.t =
    print_string
      "Transpiling Program with Inlined Monitor...\n\
       ---------- New Code ---------- \n";
    let new_prog = Prog.create_empty () in
    Hashtbl.iter
      (fun k v ->
        let (f : Func.t) = v in

        let pc = fresh_pc () in
        Printf.printf "PC: %s\n" pc;
        let new_params =
          List.fold_left
            (fun ac param -> ac @ [ param ] @ [ shadowvar param ])
            [] f.params
        in
        let asgn_vars : string list = Func.asgn_vars f.body in
        let asgn_vars_clean =
          List.fold_left
            (fun ac var ->
              if
                List.exists
                  (fun var2 -> if var = var2 then true else false)
                  new_params
              then ac
              else
                ac @ [ Stmt.Assign (shadowvar var, Expr.Val (Val.Bool true)) @@ no_region ])
            [] asgn_vars
        in
        let new_body = translist pc f.body in
        Printf.printf "Transforming function...\tFUNC NAME: %s" f.name;
        if f.name = "main" then
          let (new_f : Func.t) =
            Func.create f.name (new_params @ [ pc ])
              (Stmt.Block
                 ([
                    Stmt.AssignCall
                      ( pc,
                        Expr.Val (Val.Str "parse_lvl"),
                        [ Expr.Val (Val.Str _INITIAL_PC_) ] ) @@ no_region;
                  ]
                 @ asgn_vars_clean @ new_body ) @@ no_region)
          in
          Prog.add_func new_prog f.name new_f
        else
          let (new_f : Func.t) =
            Func.create f.name (new_params @ [ pc ])
              (Stmt.Block (asgn_vars_clean @ new_body) @@ no_region)
          in
          Prog.add_func new_prog f.name new_f)
      prog;
    print_string "------------------------------ \n\n";

    new_prog
end

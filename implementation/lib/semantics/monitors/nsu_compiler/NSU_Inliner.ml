(*Each monitor is independent of the other ones*)
exception Except of string
open NSU_CompilerConstants

module M (SL : SecurityLevel.M) = struct 


(*
let level_list (list:string list) : Expr.t=
match list with
|[]-> Expr.Val (Val.Str (SL.str (SL.get_low ())))
|_-> let ac = List.fold_left (fun ac s -> Expr.BinOpt (Oper.Lub ,ac ,(Expr.Var (shadowvar s))) ) (Expr.Val (Val.Str (SL.str (SL.get_low ())))) list in
ac
*)

let shadow_var_e (s : string) : Expr.t = Expr.Var (shadowvar s)

let binopt_e (op : Oper.bopt) (e1 : Expr.t) (e2: Expr.t) : Expr.t = Expr.BinOpt (op, e1, e2)

let bottom_e () : Expr.t =  Expr.Val (Val.Str (SL.str (SL.get_low ()))) 

let shadow_fun_e () : Expr.t = Expr.Val (Val.Str _SHADOW_FUN_NAME_)

let block_s (stmts : Stmt.t list) : Stmt.t = Stmt.Block stmts

let lub_func () : Expr.t = Expr.Val (Val.Str _LUB_)

let lubn_func () : Expr.t = Expr.Val (Val.Str _LUBN_)

let leq_func () : Expr.t = Expr.Val (Val.Str _LEQ_) 

let c_expr (e : Expr.t) : Stmt.t * string =
  let xs = Expr.vars e in
  let x_lev = fresh_expr_lev () in
  let e = Expr.NOpt (Oper.ListExpr, (List.map (fun x -> shadow_var_e x) xs)) in 
  Stmt.AssignCall (x_lev, lubn_func (), [e]), x_lev 

let prepare_args (es : Expr.t list) : Expr.t list * Stmt.t list =
  let lst = List.map (
    fun e -> 
      let (s, x) = c_expr e in
      ([e; Expr.Var x], s)) es in
  let (args, stmts) = List.split lst in
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
let c_fieldlookup (pc : string) (x : string) (e_o : Expr.t) (e_f : Expr.t) : Stmt.t list = 
 
  let (stmt_1, x_1_lev) = c_expr e_o in
  let (stmt_2, x_2_lev) = c_expr e_f in
  let x_o = fresh_obj () in
  let x_f = fresh_field () in
  let f_shadow = fresh_var () in
  let x_f_lev = fresh_field_lev () in
  let lub_1 = fresh_var_lev () in
  let lub_2 = fresh_var_lev () in
  let leq_1 = fresh_var () in
  let assigns = [
    stmt_1;
    stmt_2;
    Stmt.Assign (x_o, e_o); 
    Stmt.Assign (x_f, e_f);
    Stmt.AssignCall (f_shadow, shadow_fun_e (), [Expr.Var x_f]);
    Stmt.FieldLookup (x_f_lev, Expr.Var x_o, Expr.Var f_shadow);
    Stmt.AssignCall (lub_1, lubn_func (), [Expr.Var x_1_lev; Expr.Var x_2_lev; (Expr.Var pc)]);
    Stmt.AssignCall (leq_1, leq_func (), [Expr.Var lub_1; Expr.Var (shadowvar x)]);
    Stmt.If (Expr.Var leq_1, 
    Stmt.Block ([
      Stmt.AssignCall (lub_2, lub_func (), [Expr.Var x_f_lev; Expr.Var lub_1]);
      Stmt.Assign (shadowvar x, Expr.Var lub_2);
      FieldLookup (x, Expr.Var x_o, Expr.Var x_f)
      ]
    ), 
      Some (Stmt.Block ([
        Stmt.Exception ("Illegal Field Lookup")
        ])))] in

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
let c_if (pc : string) (pc' : string) (e : Expr.t) (stmts_1 : Stmt.t list) (stmts_2 : Stmt.t list) : Stmt.t list=
  let (stmt_e, x_e_lev) = c_expr e in
  let lub_1 = fresh_var_lev () in
  let if_stmt = 
  match stmts_2 with 
    | []  -> Stmt.If (e, block_s stmts_1, None)
    | _   -> Stmt.If (e, block_s stmts_1, Some (block_s stmts_2)) in
  [
  stmt_e;
  Stmt.AssignCall (lub_1, lub_func (), [Expr.Var pc; Expr.Var x_e_lev]);
  Stmt.Assign (pc',Expr.Var lub_1); 
  if_stmt
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
  let (stmt_e, x_e_lev) = c_expr e in
  let lub_1 = fresh_var_lev () in
  [
    stmt_e;
    Stmt.AssignCall (lub_1, lub_func (), [Expr.Var x_e_lev; Expr.Var pc]);
    Stmt.Return (Expr.NOpt (Oper.TupleExpr, [e; Expr.Var lub_1]))
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
let c_while (pc : string) (pc' : string) (e : Expr.t) (stmts : Stmt.t list): Stmt.t list =
  let (stmt_e, x_e_lev) = c_expr e in
  let while_stmt= [Stmt.While (e, Stmt.Block stmts)] in
  [
  stmt_e;
  Stmt.AssignCall (pc', lub_func (), [Expr.Var pc; Expr.Var x_e_lev]);
  ]@ while_stmt


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
let c_assign (pc : string) (x : string) (e : Expr.t) : Stmt.t list=
  let (stmt_e, x_e_lev) = c_expr e in
  let x_shadow = shadow_var_e x in
  let leq_1 = fresh_var () in
  let st1 = 
  Stmt.Block ([
    stmt_e;
    Stmt.AssignCall (shadowvar x, lub_func (), [Expr.Var pc; Expr.Var x_e_lev]); 
    Stmt.Assign (x,e)
  ]) in
  [
  Stmt.AssignCall (leq_1, leq_func (), [Expr.Var pc; x_shadow]);
  Stmt.If (Expr.Var leq_1, 
    st1 ,
    Some (Stmt.Block ([
      Stmt.Exception "Illegal Assignment"
    ])))
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
let c_assigncall (pc : string) (x : string) (f : Expr.t) (args : Expr.t list) : Stmt.t list =
  let (new_args, stmt_args) = prepare_args args in 
  let res = fresh_var () in
  let leq_1 = fresh_var () in
  let st1 = ([
    Stmt.AssignCall (res, f , (new_args @ [Expr.Var pc])); 
    Stmt.Assign (x, (Expr.UnOpt (Oper.First ,Expr.Var res)));
    Stmt.Assign (shadowvar x, (Expr.UnOpt (Oper.Second, Expr.Var res)))
  ]) in 
  stmt_args @
  [ 
    Stmt.AssignCall (leq_1, leq_func (), [Expr.Var pc; shadow_var_e x]);
    Stmt.If (Expr.Var leq_1, Stmt.Block st1, Some (Stmt.Block ([Stmt.Exception "MONITOR BLOCK - pc bigger than x"])))
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
  let (stmt_e, x_e_lev) = c_expr e in
  let leq_1 = fresh_var () in
  [
    stmt_e;
    Stmt.AssignCall (leq_1, leq_func (), [Expr.Var pc; Expr.Var x_e_lev]);
    Stmt.If (Expr.Var leq_1, 
      (Stmt.Block ([Stmt.Print e])), 
      Some (Stmt.Block ([Stmt.Exception "Illegal Print"])))
  ]


(*
------------
C(x:={})=
  x:= {};
  x["strunct_lev"] := pc;
  x["object_lev"] := pc
*)
let c_assignnewobj (pc : string) (x : string) : Stmt.t list =
  [Stmt.AssignNewObj x;
  Stmt.FieldAssign (Expr.Var x, Expr.Var _OBJ_STRUCT_LEV_PROP_, Expr.Var pc);
  Stmt.FieldAssign (Expr.Var x, Expr.Var _OBJ_VALUE_LEV_PROP_, Expr.Var pc)]


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
    x_o[prop_exists_lev_name] := ctx
    } else{
      throw("Illegal Field Creation")
    }
  }
*)
let c_fieldassign (pc : string) (e_o : Expr.t) (e_f : Expr.t) (e_v : Expr.t) : Stmt.t list =
  let (stmt_x_o, x_o_lev) = c_expr e_o in
  let (stmt_x_f, x_f_lev) = c_expr e_f in
  let (stmt_x_v, x_v_lev) = c_expr e_v in
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
    Stmt.AssignCall (ctx, lubn_func (), [Expr.Var x_o_lev; Expr.Var x_f_lev; (Expr.Var pc)]);
    Stmt.AssignCall(prop_val_lev_name, Expr.Val (Val.Str _SHADOW_PROP_VALUE_), [x_f]);
    Stmt.FieldLookup(prop_val_lev, x_o, Expr.Var prop_val_lev_name);
    Stmt.If (binopt_e Oper.Equal (Expr.Var prop_val_lev) (Expr.Val (Val.Null)),
      Stmt.Block ([ 
        Stmt.FieldLookup (struct_lev, x_o, (Expr.Var _OBJ_STRUCT_LEV_PROP_));
        Stmt.AssignCall (leq_1, leq_func (), [Expr.Var ctx; Expr.Var struct_lev]);
        Stmt.If(Expr.Var leq_1,
          Stmt.Block([
            Stmt.AssignCall (prop_exists_lev_name, Expr.Val (Val.Str _SHADOW_PROP_EXISTS_), [x_f]);
            Stmt.AssignCall (lub_1, lub_func (), [Expr.Var ctx; Expr.Var x_v_lev]); 
            Stmt.FieldAssign (x_o, Expr.Var prop_val_lev_name, Expr.Var lub_1);
            Stmt.FieldAssign (x_o, Expr.Var prop_exists_lev_name, Expr.Var ctx)]) ,
          Some ( Stmt.Block [
        Stmt.Exception ("Illegal Field Creation")]))
      ]),Some (Stmt.Block ([
        Stmt.AssignCall (leq_1, leq_func (), [Expr.Var ctx; Expr.Var prop_val_lev]);
        Stmt.If (Expr.Var leq_1, 
          Stmt.Block ([
            Stmt.AssignCall (lub_1, lubn_func (), [Expr.Var ctx; Expr.Var x_v_lev]);
            Stmt.FieldAssign (x_o, Expr.Var prop_val_lev_name, Expr.Var lub_1);
            Stmt.FieldAssign (x_o, x_f, x_v)
          ]), Some (Stmt.Block ([
            Stmt.Exception ("Illegal Field Assign")
          ])))])))
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
  let (stmt_x_o, x_o_lev) = c_expr e_o in
  let (stmt_x_f, x_f_lev) = c_expr e_f in
  let x_o = e_o in
  let x_f = e_f in
  let ctx = fresh_var () in
  let leq_1 = fresh_var () in
  let prop_exists_lev_name = fresh_var () in
  let prop_exists_lev = fresh_field_lev () in
  [
   stmt_x_o;
   stmt_x_f;
   Stmt.AssignCall (ctx, lubn_func (), [Expr.Var x_o_lev; Expr.Var x_f_lev; Expr.Var pc ]);
   Stmt.AssignCall (prop_exists_lev_name, Expr.Val (Val.Str _SHADOW_PROP_EXISTS_), [x_f]);
   Stmt.FieldLookup (prop_exists_lev, x_o, Expr.Var prop_exists_lev_name);
   Stmt.If(binopt_e Oper.Equal (Expr.Var prop_exists_lev) (Expr.Val (Val.Null)),
   Stmt.Block [
    Stmt.Exception("Internal Error")], 
   Some(Stmt.Block [
    Stmt.AssignCall (leq_1, leq_func (), [Expr.Var ctx; Expr.Var prop_exists_lev]);
    Stmt.If (Expr.Var leq_1, 
    Stmt.Block ([
      Stmt.FieldDelete (x_o, x_f)]),
    Some (Stmt.Block ([
      Stmt.Exception ("Illegal Field Delete")
   ])))]))

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
let c_assinginobjcheck (pc : string) (x : string) (e_f : Expr.t) (e_o : Expr.t) : Stmt.t list =
  let (stmt_x_o, x_o_lev) = c_expr e_o in
  let (stmt_x_f, x_f_lev) = c_expr e_f in
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
    Stmt.AssignCall (ctx, lubn_func (), [Expr.Var x_o_lev; Expr.Var x_f_lev; Expr.Var pc]);
    Stmt.AssignCall (prop_exists_lev_name,Expr.Val (Val.Str _SHADOW_PROP_EXISTS_), [x_f]);
    Stmt.FieldLookup (prop_exists_lev, x_o, Expr.Var prop_exists_lev_name);
    Stmt.FieldLookup (struct_lev, x_o, (Expr.Var _OBJ_STRUCT_LEV_PROP_));
    Stmt.AssignCall (leq_1, leq_func (), [Expr.Var ctx; shadow_var_e x]);
    Stmt.If (Expr.Var leq_1, 
    Stmt.Block ([
      Stmt.If (binopt_e Oper.Equal (Expr.Var prop_exists_lev) (Expr.Val (Val.Null)),
        Stmt.AssignCall (shadowvar x, lub_func (), [Expr.Var ctx; Expr.Var struct_lev]),
        Some ( 
        Stmt.AssignCall (shadowvar x, lub_func (), [Expr.Var ctx; Expr.Var prop_exists_lev])))
    ]), Some ( Stmt.Exception "Illegal Assignment"));
    Stmt.AssignInObjCheck (x, x_f, x_o)
  ]


(*
  x_shadow = shadow(x)
  ret_shadow = shadow(ret)
  leq_1 lub_1 fresh
  __________________________
  C(ret := upgVar(x, lev_str))=
    lev := parse_lvl(lev_str);
    x_shadow := lev;
    ret_shadow := pc;
    ret := null


*)
let c_upgVar (pc : string) (ret : string) (x_name : string) (lev_str : string) : Stmt.t list =
  let x_shadow = shadowvar x_name in
  let ret_shadow = shadowvar ret in
  let leq_1 = fresh_var () in
    [
      Stmt.AssignCall (leq_1, leq_func (), [Expr.Var pc; shadow_var_e x_name]);
      Stmt.If (Expr.Var leq_1,
          Stmt.AssignCall (x_shadow, lub_func (), [shadow_var_e x_name; Expr.Var pc]),
          Some (
            Stmt.Exception "Illegal UpgVarLab"
          )
        );
      Stmt.Assign (ret_shadow, Expr.Var pc);
      Stmt.Return (Expr.NOpt (Oper.TupleExpr, [Expr.Val (Val.Null); Expr.Var ret_shadow]))
    ]  





let rec compile (pc:string) (stmt: Stmt.t ): Stmt.t list=

  let compile_o pc = Option.map (compile pc) in

  match stmt with
  	| Block lst -> c_block pc lst

    | Skip ->  [Stmt.Skip]

    | Assign (x,e) -> c_assign pc x e

    | Return e -> c_return pc e

    | If (e,s1,s2) -> 
      let pc' = fresh_var () in
      let stmts_1 = compile pc' s1 in
      let stmts_2 = Option.default [] (compile_o pc' s2) in
      c_if pc pc' e stmts_1 stmts_2
    
    | While (e,s) ->
      let pc' = fresh_var () in
      let stmts = compile pc' s in
      c_while pc pc' e stmts

    | AssignCall (ret, Expr.Val (Val.Str f), [Expr.Val (Val.Str x_name); Expr.Val (Val.Str lev_str)]) when f = _upgVar_ -> c_upgVar pc ret x_name lev_str

    | AssignCall (x,f,args) -> c_assigncall pc x f args

    | Print (e) -> c_print pc e

    | AssignInObjCheck (st, e_f, e_o) -> c_assinginobjcheck pc st e_f e_o
     
    | AssignNewObj (x) -> c_assignnewobj pc x 
     
    | FieldLookup (x, e_o, e_f) -> c_fieldlookup pc x e_o e_f

    | FieldAssign (e_o, e_f, e_v) -> c_fieldassign pc e_o e_f e_v

    | FieldDelete (e_o, e_f) -> c_fielddelete pc e_o e_f
     
    | AssignObjToList (st, e) -> [Stmt.AssignObjToList (st,e)]  

    | AssignObjFields (st, e) -> [Stmt.AssignObjFields (st,e)]

    |_ ->		raise(Except ("Unknown Op -> " ^ Stmt.str stmt))(*ERROR*)

and c_block (pc : string) (lst : Stmt.t list) : Stmt.t list =
  let res : Stmt.t list = List.fold_left (fun ac stmt -> ac @ (compile pc  stmt)) [] lst in 
      res


let translist (pc:string) (_stmts: Stmt.t ) : Stmt.t list=
  match _stmts with
  | Block stmts -> List.fold_left (fun ac s -> ac @ compile pc s) [] stmts
  | _ -> [] 






let compile_functions (prog : Prog.t) (out_file : string): Prog.t =
  print_string ("Transpiling Program with Inlined Monitor...\n---------- New Code ---------- \n\n\n");
  let new_prog = Prog.create_empty () in
  Hashtbl.iter (fun k v ->  let (f: Func.t)= v in
                            let pc = fresh_pc () in
                            let new_params = List.fold_left (fun ac param-> ac @ [param] @ [shadowvar param] ) [] f.params in
                            let asgn_vars: string list = Func.asgn_vars (f.body) in
                            let asgn_vars_clean = List.fold_left (fun ac var ->  if (List.exists (fun var2 -> if var = var2 then true else false ) new_params) then ac else ac @ [Stmt.Assign (shadowvar var, (Expr.Var pc))] ) [] asgn_vars in

                            let new_body= translist pc (f.body) in

                            let (new_f : Func.t) = Func.create f.name (new_params @ [pc])  (Stmt.Block (asgn_vars_clean @ new_body)) in
                            print_string ((Func.str new_f)^"\n\n");
                            Prog.add_func new_prog f.name  new_f
  ) prog;
  print_string ("\n------------------------------ \n\n");
  if(out_file = "") then () else(
    let oc = open_out out_file in    (* create or truncate file, return channel *)
      print_string ("Printing output to : "^out_file^"...\nNumber of functions : "^string_of_int (Hashtbl.length new_prog)^"\n");
      Printf.fprintf oc "%s\n" (Prog.str new_prog);   (* write something *)
      close_out oc
  );
  new_prog


end 

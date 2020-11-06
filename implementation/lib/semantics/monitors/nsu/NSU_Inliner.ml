(*Each monitor is independent of the other ones*)
exception Except of string

module M (SL : SecurityLevel.M) = struct 

let _SHADOW_FUN_NAME_ = "shadow"

let mk_fresh_var (str : string) : (unit -> string)=
  let counter = ref 0 in
  let rec f () =
    (let v = str ^ (string_of_int !counter) in
    counter := !counter +1;
    v)
  in f

let  fresh_var  = mk_fresh_var ("_freshvar_")
let  fresh_obj  = mk_fresh_var ("_freshobj_")
let  fresh_field  = mk_fresh_var ("_freshfield_")
let  fresh_field_lev  = mk_fresh_var ("_freshfield_lev_")

let mk_fresh_pc (): (unit -> string)=
  let counterpc = ref 1 in
  let rec f () =
    (let v = "_pc_" ^ (string_of_int !counterpc) in
    counterpc := !counterpc +1;
    v)
  in f

let  fresh_pc  = mk_fresh_pc ()

let level_list (list:string list) : Expr.t=
match list with
|[]-> Expr.Val (Val.Str (SL.str (SL.get_low ())))
|_-> let ac = List.fold_left (fun ac s -> Expr.BinOpt (Oper.Lub ,ac ,(Expr.Var (Val.shadowvar s))) ) (Expr.Val (Val.Str (SL.str (SL.get_low ())))) list in
ac


let shadow_var_e (s : string) : Expr.t = Expr.Var (Val.shadowvar s)

let binopt_e (op : Oper.bopt) (e1 : Expr.t) (e2: Expr.t) : Expr.t = Expr.BinOpt (op, e1, e2)

let bottom_e () : Expr.t =  Expr.Val (Val.Str (SL.str (SL.get_low ()))) 

let shadow_fun_e () : Expr.t = Expr.Val (Val.Str _SHADOW_FUN_NAME_)

let block_s (stmts : Stmt.t list) : Stmt.t = Stmt.Block stmts

let c_expr (e : Expr.t) : Expr.t =
  let xs = Expr.vars e in
  match xs with
    |[]-> bottom_e ()
    |_-> 
      List.fold_left 
        (fun ac s -> binopt_e Oper.Lub ac (shadow_var_e s))
        (bottom_e ())
        xs 
    

(*
C_exp(e1) = e1_l
C_exp(e2) = e2_l
x_o, x_p fresh
-------------------------------
C(x := e1[e2]) = 
   x_o := e1; 
   x_p := e2; 
   p_shadow := shadow(x_p); 
   x_p_l := x_o[p_shadow]; 
   lev_ctx := lub(pc, e1_l, e2_l);
   if (leq(lev_ctx, x_lev)) { 
      x_lev := lub(x_p_l, lev_ctx); 
      x := x_o[x_p]
   } else { 
       throw("IFlow Exception")  
   }
*)
let c_fieldlookup (pc : string) (x : string) (e_o : Expr.t) (e_f : Expr.t) : Stmt.t list = 
  print_string "FieldLookup\n";

  let e_o_lev = c_expr e_o in
  let e_f_lev = c_expr e_f in
  let x_o = fresh_obj () in
  let x_f = fresh_field () in
  let f_shadow = fresh_var () in
  let x_f_lev = fresh_field_lev () in 
  let ctx1 = binopt_e Oper.Lub e_o_lev e_f_lev in
  let ctx = binopt_e Oper.Lub (Expr.Var pc) ctx1 in
  let assigns = [
    Stmt.Assign (x_o, e_o); 
    Stmt.Assign (x_f, e_f);
    Stmt.AssignCall (f_shadow, shadow_fun_e (), [Expr.Var x_f]);
    Stmt.FieldLookup (x_f_lev, Expr.Var x_o, Expr.Var f_shadow);
    Stmt.If (Expr.BinOpt (Oper.Leq, ctx, Expr.Var (Val.shadowvar x)), 
    Stmt.Block ([
      Stmt.Assign (Val.shadowvar x, Expr.BinOpt (Oper.Lub, ctx, Expr.Var x_f_lev));
      FieldLookup (x, Expr.Var x_o, Expr.Var x_f)
      ]
   ), 
    Some (Stmt.Block ([
      Stmt.Exception ("Illegal Field Lookup")
      ])))] in

    assigns


(*
pc' fresh
C_exp(e) = e_l
C(s1, pc') = stmts_1
C(s2, pc') = stmts_2
--------------------
C(if(e){s1} else {s2}) =
  pc' := lub(pc, e_l);
  if(e){stmts_1} else {stmts_2}
*)
let c_if (pc : string) (pc' : string) (e : Expr.t) (stmts_1 : Stmt.t list) (stmts_2 : Stmt.t list) : Stmt.t list=
  let e_lev = c_expr e in
  let e_ctx = binopt_e Oper.Lub  (Expr.Var pc) e_lev in
  let if_stmt = 
  match stmts_2 with 
    | []  -> Stmt.If (e, block_s stmts_1, None)
    | _   -> Stmt.If (e, block_s stmts_1, Some (block_s stmts_2)) in
  [Stmt.Assign (pc',e_ctx); if_stmt]


(*
  C_exp(e)= e_l
  ret_lev fresh
  --------------
  C(return e)= 
    ret_lev := lub(pc, e_l);
    return (e, ret_lev)
*)
let c_return (pc : string) (e : Expr.t) : Stmt.t list = 
  let e_lev = c_expr e in
  let ret_lev = binopt_e Oper.Lub e_lev (Expr.Var pc) in
  [Stmt.Return (Expr.Pair (e, ret_lev))]


(*
  pc' fresh
  C_exp(e)= e_l
  C(s, pc') = stmts
  ---------------------
  C(while(e){s1})=
    pc' := lub (pc, e_l);
    while(e){stmts}
*)
let c_while (pc : string) (pc' : string) (e : Expr.t) (stmts : Stmt.t list): Stmt.t list =
  let e_lev = c_expr e in 
  let e_ctx = binopt_e Oper.Lub  (Expr.Var pc) e_lev in
  let while_stmt= [Stmt.While (e, Stmt.Block stmts)] in
  [Stmt.Assign ( pc',e_ctx)]@ while_stmt


(*
  C_exp(e)= e_l
  -------------
  C(x := e)=
    ctx_lev := lub(pc, e_l);
    if(leq(pc, x_lev)){
      x_lev := lub(pc, e_l);
      x := e
    } else {
      throw("IFlow Exception")
    }
*)
let c_assign (pc : string) (x : string) (e : Expr.t) : Stmt.t list=
  let e_lev = c_expr e in
  let x_shadow = shadow_var_e x in
  let cond = binopt_e Oper.Leq (Expr.Var pc) x_shadow in
  let st1 = Stmt.Block ([Stmt.Assign(Val.shadowvar x,(Expr.BinOpt (Oper.Lub,  (Expr.Var pc), e_lev))); Stmt.Assign (x,e)]) in
  let out= (Stmt.If (cond ,st1 ,Some (Stmt.Block ([Stmt.Exception "Illegal Assignment"])))) in
  [out]


(*
  res fresh
  ----------------
  C(x := f(args))= 
    new_args := prepare_args(args);
    if(leq(pc, x_lev)){
      res := f(new_args, pc);
      x := first(res);
      x_lev := second(res);
    } else {
      throw("MONITOR BLOCK - pc bigger than x")
    }   
*)
let c_assigncall (pc : string) (x : string) (f : Expr.t) (args : Expr.t list) : Stmt.t list =
  

       let new_args = List.fold_left (fun ac e-> ( ac@ [e] @[(c_expr e)])) [] args in
       let res = fresh_var () in
        let cond = binopt_e Oper.Leq (Expr.Var pc) (shadow_var_e x) in (*PC <= x_lev*)
        let code= ([Stmt.AssignCall (res, f , (new_args @ [Expr.Var pc])); 
                  Stmt.Assign (x, (Expr.UnOpt (Oper.First ,Expr.Var res)));
                  Stmt.Assign (Val.shadowvar x, (Expr.UnOpt (Oper.Second, Expr.Var res)))]) in
        let st1 = [Stmt.Assign (res, Expr.Val(Val.Str "empty"))] @ code in
        [Stmt.If (cond, Stmt.Block st1, Some (Stmt.Block ([Stmt.Exception "MONITOR BLOCK - pc bigger than x"])))]


(*
  C(e)= e_lev
  -----------
  C(print(e))=
  if(leq(pc, e_lev)){
    print(e)
  } else {
   throw("Illegal Print")
  }

*)
let c_print (pc : string) (e : Expr.t) : Stmt.t list =
  let e_lev = c_expr e in
  let cond = binopt_e Oper.Leq (Expr.Var pc) e_lev in 
  let out = Stmt.If (cond, 
            (Stmt.Block ([Stmt.Print e])), 
            Some (Stmt.Block ([Stmt.Exception "Illegal Print"])))   in
  [out]


(*
------------
C(x:={})=
  x:= {};
  x["strunct_lev"] := pc;
  x["object_lev"] := pc
*)
let c_assignnewobj (pc : string) (x : string) : Stmt.t list =
  [Stmt.AssignNewObj x;
  Stmt.FieldAssign (Expr.Var x, Expr.Var ("struct_lev"), Expr.Var pc);
  Stmt.FieldAssign (Expr.Var x, Expr.Var ("object_lev"), Expr.Var pc);]


(*
C(e_o) = e_o_lev
C(e_f) = e_f_lev
C(e_v) = e_v_lev
prop_val_lev_name prop_val_lev fresh
prop_exists_lev_name fresh
struct_lvl_name struct_lvl fresh
-------------------------------------
C(e_o[e_f]:= e_v)=
  ctx := lub(lub(e_o_lev, e_f_lev), pc);
  prop_val_lev_name := propVal(e_f);
  prop_val_lev := e_o[prop_val_lev_name];
  if(prop_val_lev != null){
    if(leq(ctx, prop_val_lev)){
      e_o[prop_val_lev_name] := lub(ctx, e_v_lev);
      e_o[e_f] := e_v;
    } else {
      throw("Illegal Field Assign")
    }
  } else{
    struct_lev_name := structLevName();
    struct_lev := e_o[struct_lev_name];
    if(leq(ctx, struct_lev)){
    prop_exists_lev_name := propExists(e_f);
    e_o[prop_val_lev_name] := lub(ctx, e_v_lev)
    e_o[prop_exists_lev_name] := ctx
    } else{
      throw("Illegal Field Creation")
    }
  }
*)
let c_fieldassign (pc : string) (e_o : Expr.t) (e_f : Expr.t) (e_v : Expr.t) : Stmt.t list =
  (*E SE A PROP NAO EXISTIR?*)
  let e_o_lev = c_expr e_o in
  let e_f_lev = c_expr e_f in
  let e_v_lev = c_expr e_v in
  let ctx1 = binopt_e Oper.Lub e_o_lev e_f_lev in
  let ctx = binopt_e Oper.Lub (Expr.Var pc) ctx1 in
  let prop_val_lev_name = fresh_var () in
  let prop_val_lev = fresh_field_lev () in
  let prop_exists_lev_name = fresh_var () in
  let prop_exists_lev = fresh_field_lev () in
  let struct_lev = fresh_field_lev () in
   [
    Stmt.AssignCall(prop_val_lev_name, Expr.Val (Val.Str "propVal"), [e_f]);
    Stmt.FieldLookup(prop_val_lev, e_o, Expr.Var prop_val_lev_name);
    Stmt.If (binopt_e Oper.Equal (Expr.Var prop_val_lev) (Expr.Val (Val.Null)),
      Stmt.Block ([ 
        Stmt.FieldLookup(struct_lev, e_o, (Expr.Var "struct_lev"));
        Stmt.If(binopt_e Oper.Leq ctx (Expr.Var struct_lev),
          Stmt.Block([
            Stmt.AssignCall(prop_exists_lev_name, Expr.Val (Val.Str "propExists"), [e_f]);
            Stmt.FieldAssign(e_o, Expr.Var prop_val_lev_name, binopt_e Oper.Lub ctx e_v_lev);
            Stmt.FieldAssign(e_o, Expr.Var prop_exists_lev_name, ctx)]) ,
          Some ( Stmt.Block [
        Stmt.Exception ("Illegal Field Creation")]))
      ]),Some (Stmt.Block ([
        Stmt.If (binopt_e Oper.Leq ctx (Expr.Var prop_val_lev), 
          Stmt.Block ([
            Stmt.FieldAssign (e_o, Expr.Var prop_val_lev_name, (binopt_e Oper.Lub ctx e_v_lev));
            Stmt.FieldAssign (e_o, e_f, e_v)
          ]), Some (Stmt.Block ([
            Stmt.Exception ("Illegal Field Assign")
          ])))])))
    ]


(*
C(e_o) = e_o_lev
C(e_f) = e_f_lev
prop_exists_lev prop_exists_lev_name fresh
------------------------------------------
C(delete(e_o[e_f]))=
  ctx=lub(lub(e_o_lev, e_f_lev), pc);
  prop_exists_lev_name := propExists(e_f);
  prop_exists_lev := e_o[prop_exists_lev_name];
  if(prop_exists_lev == null){
    if(leq(ctx, prop_exists_lev)){
      delete(e_o[e_f])
    } else{
      throw("Illegal Field Delete")
    }
  } else {
    throw("Internal Error")
  }
*)
let c_fielddelete (pc : string) (e_o : Expr.t) (e_f : Expr.t) : Stmt.t list =
  let e_o_lev = c_expr e_o in
  let e_f_lev = c_expr e_f in
  let ctx1 = binopt_e Oper.Leq e_o_lev e_f_lev in
  let ctx = binopt_e Oper.Leq (Expr.Var pc) ctx1 in
  let prop_exists_lev_name = fresh_var () in
  let prop_exists_lev = fresh_field_lev () in
  [
   Stmt.AssignCall (prop_exists_lev_name, Expr.Val (Val.Str "propExists"), [e_f]);
   Stmt.FieldLookup (prop_exists_lev, e_o, Expr.Var prop_exists_lev_name);
   Stmt.If(binopt_e Oper.Equal (Expr.Var prop_exists_lev) (Expr.Val (Val.Null)),
   Stmt.Block [
    Stmt.Exception("Internal Error")], 
   Some(Stmt.Block [
    Stmt.If (binopt_e Oper.Leq ctx (Expr.Var prop_exists_lev), 
    Stmt.Block ([
      Stmt.FieldDelete (e_o, e_f)]),
    Some (Stmt.Block ([
      Stmt.Exception ("Illegal Field Delete")
   ])))]))

  ]


(*
  C(e_o)= e_o_lev
  C(e_f)= e_f_lev
  struct_lev fresh
  prop_exists_lev prop_exists_lev_name fresh
  ----------------------
  C(x := e_f in(e_o)) =
  ctx := lub(lub(e_o_lev, e_f_lev), pc);
  struct_lev_name := structLevName();
  struct_lev := e_o[struct_lev_name];
  prop_exists_lev_name := propExists(e_f);
  prop_exists_lev := e_o[prop_exists_lev_name];
  if(prop_exists_lev == null){
    if(x_lev == null){
      x_lev := lub(ctx, struct_lev);
    } else{
      if(leq(ctx, x_lev)){
        x_lev := lub(ctx, struct_lev);    
      }else {
        throw("Illegal Assignment")
      }
    }
  }else{
    if(x_lev == null){
      x_lev := lub(prop_exists_lev, ctx)
    } else{
      if(leq(ctx, x_lev)){
        x_lev := lub(prop_exists_lev, ctx)
      } else{ 
        throw("Illegal Assignment")
      }
    }
  }
  x := e_f in(e_o);

*)
let c_assinginobjcheck (pc : string) (x : string) (e_f : Expr.t) (e_o : Expr.t) : Stmt.t list =
  let e_o_lev = c_expr e_o in
  let e_f_lev = c_expr e_f in
  let ctx1 = binopt_e Oper.Leq e_o_lev e_f_lev in
  let ctx = binopt_e Oper.Leq (Expr.Var pc) ctx1 in
  let prop_exists_lev_name = fresh_var () in
  let prop_exists_lev = fresh_field_lev () in
  let struct_lev = fresh_field_lev () in
  [
   Stmt.AssignCall(prop_exists_lev_name,Expr.Val (Val.Str "propExists"), [e_f]);
   Stmt.FieldLookup (prop_exists_lev, e_o, Expr.Var prop_exists_lev_name);
   Stmt.FieldLookup (struct_lev, e_o, (Expr.Var "struct_lev"));
   Stmt.If (binopt_e Oper.Equal (Expr.Var prop_exists_lev) (Expr.Val (Val.Null)), 
   Stmt.Block ([
      Stmt.If(binopt_e Oper.Equal (shadow_var_e x) (Expr.Val (Val.Null)),
      Stmt.Block [
        Stmt.Assign (Val.shadowvar x, binopt_e Oper.Lub ctx (Expr.Var struct_lev))
      ], Some(Stmt.Block [
        Stmt.If(binopt_e Oper.Leq ctx (Expr.Var struct_lev),
        Stmt.Block [
          Stmt.Assign (Val.shadowvar x, binopt_e Oper.Lub ctx (Expr.Var struct_lev))
        ],
        Some (Stmt.Block [
          Stmt.Exception "Illegal Assignment"
        ]))   
      ]))
    ]), Some (Stmt.Block ([
      Stmt.If(binopt_e Oper.Equal (shadow_var_e x) (Expr.Val (Val.Null)),
      Stmt.Block [
        Stmt.Assign (Val.shadowvar x, binopt_e Oper.Lub ctx (Expr.Var prop_exists_lev))
      ], Some (Stmt.Block [
        Stmt.If (binopt_e Oper.Leq ctx (shadow_var_e x),
        Stmt.Block [
          Stmt.Assign (Val.shadowvar x, binopt_e Oper.Lub ctx (Expr.Var prop_exists_lev))
        ], Some (Stmt.Block [
          Stmt.Exception "Illegal Assignment"
        ]))
      ]))  
    ]))) 

  ]




let rec compile (pc:string) (stmt: Stmt.t ): Stmt.t list=
  let compile_lst pc = List.map (compile pc) in
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

    | AssignCall (x,f,args) -> c_assigncall pc x f args

    | Print (e) -> c_print pc e

    | AssignInObjCheck (st, e_f, e_o) -> c_assinginobjcheck pc st e_f e_o
     
    | AssignNewObj (x) -> c_assignnewobj pc x 
     
    | FieldLookup (x, e_o, e_f) -> c_fieldlookup pc x e_o e_f

    | FieldAssign (e_o, e_f, e_v) -> c_fieldassign pc e_o e_f e_v

    | FieldDelete (e_o, e_f) -> c_fielddelete pc e_o e_f
     
    | AssignObjToList (st, e) -> print_string "FieldDelete"; [Stmt.Skip]  

    | AssignObjFields (st, e) -> print_string "FieldDelete";  [Stmt.Skip]

    |_ ->		raise(Except ("Unknown Op -> " ^ Stmt.str stmt))(*ERROR*)

and c_block (pc : string) (lst : Stmt.t list) : Stmt.t list =
  let res : Stmt.t list = List.fold_left (fun ac stmt -> ac @ (compile pc  stmt)) [] lst in 
      res


let translist (pc:string) (_stmts: Stmt.t ) : Stmt.t list=
match _stmts with
Block stmts -> List.fold_left (fun ac s -> ac @ compile pc s) [] stmts






let compile_functions (prog : Prog.t) (out_file : string): Prog.t =
  print_string ("Transpiling Program with Inlined Monitor...\n---------- New Code ---------- \n\n\n");
  let new_prog = Prog.create_empty () in
  Hashtbl.iter (fun k v ->  let (f: Func.t)= v in
                            let pc = fresh_pc () in
                            let new_params = List.fold_left (fun ac param-> ac @ [param] @ [Val.shadowvar param] ) [] f.params in
                            let asgn_vars: string list = Func.asgn_vars (f.body) in
                            let asgn_vars_clean = List.fold_left (fun ac var ->  if (List.exists (fun var2 -> if var = var2 then true else false ) new_params) then ac else ac @ [Stmt.Assign (Val.shadowvar var, (Expr.Var pc))] ) [] asgn_vars in

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

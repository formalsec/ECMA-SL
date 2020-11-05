(*Each monitor is independent of the other ones*)
exception Except of string

module M (SL : SecurityLevel.M) = struct 

let mk_fresh_var (): (unit -> string)=
  let counter = ref 0 in
  let rec f () =
    (let v = "_freshvariable_" ^ (string_of_int !counter) in
    counter := !counter +1;
    v)
  in f

let  fresh_var  = mk_fresh_var ()

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





let rec compile (pc:string) (stmt: Stmt.t ): Stmt.t list=

  match stmt with
  	| Block lst -> c_block pc lst

    | Skip ->  [Stmt.Skip]

    | Assign (x,e) -> c_assign pc x e

    | Return e -> c_return pc e

    | If (e,s1,s2) -> c_if pc e s1 s2
    
    | While (e,s) -> c_while pc e s

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


and translist (pc:string) (_stmts: Stmt.t ) : Stmt.t list=
match _stmts with
Block stmts -> List.fold_left (fun ac s -> ac @ compile pc s) [] stmts


and c_block (pc : string) (lst : Stmt.t list) : Stmt.t list =
  let res : Stmt.t list = List.fold_left (fun ac stmt -> ac @ (compile pc  stmt)) [] lst in 
      res

and c_return (pc : string) (e : Expr.t) : Stmt.t list = 
  let vars = Expr.vars e in
    [Stmt.Return (Expr.Pair (e,Expr.BinOpt (Oper.Lub ,(level_list vars),  (Expr.Var pc) )))]

and c_while (pc : string) (e : Expr.t) (s : Stmt.t): Stmt.t list =
  let vars = Expr.vars e in
                     let pc' = fresh_pc () in
                     let pc_value = Expr.BinOpt (Oper.Lub,  (Expr.Var pc) , (level_list vars)) in
                     let s'= translist pc' s in
                     let while_stmt= [Stmt.While (e, Stmt.Block s')]    in
                      [Stmt.Assign ( pc',pc_value)]@ while_stmt

and c_assign (pc : string) (x : string) (e : Expr.t) : Stmt.t list=
  print_string "Assign\n";
                    let vars = Expr.vars e in
                    let cond = Expr.BinOpt (Oper.Leq , (Expr.Var  (Val.shadowvar x)),  (Expr.Var pc) )in (*PC <= x_lev*)
                    let fresh = fresh_var () in
                    let st1 = Stmt.Block ([Stmt.Assign((Val.shadowvar x),(Expr.BinOpt (Oper.Lub,  (Expr.Var pc), (level_list vars))))] @ [Stmt.Assign (x,e)]) in
                    let out= (Stmt.If (cond ,st1 ,Some (Stmt.Block ([Stmt.Exception "MONITOR BLOCK - pc bigger than x"])))) in
                    [out]

and c_if (pc : string) (e : Expr.t) (s1 : Stmt.t) (s2 : Stmt.t option) : Stmt.t list=
  let pc' = fresh_pc () in
          let vars = Expr.vars e in
          let pc_value = Expr.BinOpt (Oper.Lub,  (Expr.Var pc), (level_list vars)) in
          let cond = e in
          let s1'= translist  pc' s1 in
          (match s2 with 
          | Some s2 ->
            let s2' = translist  pc' s2 in
            let if_stmt= [Stmt.If (cond ,  Stmt.Block s1' ,   Some (Stmt.Block s2') )]  in
              [Stmt.Assign (pc',pc_value)] @ if_stmt
          | None ->
            let if_stmt= [Stmt.If (cond ,  Stmt.Block s1' ,   None )]  in
              [Stmt.Assign (pc',pc_value)] @ if_stmt)

and c_assigncall (pc : string) (x : string) (f : Expr.t) (args : Expr.t list) : Stmt.t list =
  
                          (*- Utilizacao de Pairs (x,xlev)? *)
                          (*- Ao fazer o retorno como Pair significa que nao se vao poder utilizar valores do tipo pair*)
                         let new_args = List.fold_left (fun ac e-> ( let vars = Expr.vars e in
                                                                    ac@ [e] @[(level_list vars)]
                                                                    )) [] args in
                         let fresh = fresh_var () in
                         let x_name = x in
                          let cond = Expr.BinOpt (Oper.Leq ,(Expr.Var  (Val.shadowvar x_name)),  (Expr.Var pc) )in (*PC <= x_lev*)
                          let code= ([Stmt.AssignCall ((fresh), f , (new_args @ [Expr.Var pc] ))] @ [Stmt.Assign (x_name, (Expr.UnOpt (Oper.First ,Expr.Var fresh)))]  @ [Stmt.Assign (Val.shadowvar x_name, (Expr.UnOpt (Oper.Second, Expr.Var fresh)))]) in
                          let st1 = [Stmt.Assign (fresh, Expr.Val(Val.Str "empty"))]@ code in

                          [Stmt.If (cond, Stmt.Block st1, Some (Stmt.Block ([Stmt.Exception "MONITOR BLOCK - pc bigger than x"])))]

and c_fieldlookup (pc : string) (x : string) (e_o : Expr.t) (e_f : Expr.t) : Stmt.t list = 
  print_string "FieldLookup\n";
  let e_o_vars = Expr.vars e_o in
  let e_f_vars = Expr.vars e_f in
  let e_o_lev = level_list e_o_vars in
  let e_f_lev = level_list e_f_vars in
  let ctx1 = Expr.BinOpt (Oper.Leq, e_o_lev, e_f_lev) in
  let ctx = Expr.BinOpt (Oper.Leq, Expr.Var pc, ctx1) in
  let assigns = [Stmt.Assign ("x_o", e_o); 
                 Stmt.Assign ("x_f", e_f);
                 Stmt.AssignCall ("p_shadow", Expr.Val (Val.Str "shadow"), [Expr.Var "x_p"]);
                 Stmt.FieldLookup ("x_f_lev", Expr.Var ("x_o"), Expr.Var ("p_shadow") );
                 Stmt.If (Expr.BinOpt (Oper.Leq, ctx, Expr.Var (Val.shadowvar x)), 
                  Stmt.Block ([
                    Stmt.Assign (Val.shadowvar x, Expr.BinOpt (Oper.Lub, ctx, Expr.Var "x_f_lev"));
                    FieldLookup (x, e_o, e_f)
                    ]
                 ), 
                  Some (Stmt.Block ([
                    Stmt.Exception ("Illegal Field Lookup")
                    ])))] in

    assigns

and c_print (pc : string) (e : Expr.t) : Stmt.t list =
  let vars = Expr.vars e in
                 let cond = Expr.BinOpt (Oper.Lub ,(level_list vars),  (Expr.Var pc) ) in 
                 let out = Stmt.If (cond, (Stmt.Block ([Stmt.Print e])), Some (Stmt.Block ([Stmt.Exception "MONITOR BLOCK - Illegal Output"])))   in
                 [out]

and c_assignnewobj (pc : string) (x : string) : Stmt.t list =
  print_string "AssignNewObj\n";
  [

  Stmt.Assign (Val.shadowvar x, Expr.Var pc);
  Stmt.AssignNewObj x;
  Stmt.FieldAssign (Expr.Var x, Expr.Var ("struct_lev"), Expr.Var pc);
  Stmt.FieldAssign (Expr.Var x, Expr.Var ("object_lev"), Expr.Var pc);
  ]

and c_fieldassign (pc : string) (e_o : Expr.t) (e_f : Expr.t) (e_v : Expr.t) : Stmt.t list =
  (*E SE A PROP NAO EXISTIR?*)
  print_string "FieldAssign\n";
  let e_o_vars = Expr.vars e_o in
  let e_f_vars = Expr.vars e_f in
  let e_v_vars = Expr.vars e_v in
  let e_o_lev = level_list e_o_vars in
  let e_f_lev = level_list e_f_vars in
  let e_v_lev = level_list e_v_vars in
  let ctx1 = Expr.BinOpt (Oper.Leq, e_o_lev, e_f_lev) in
  let ctx = Expr.BinOpt (Oper.Leq, Expr.Var pc, ctx1) in
   [
    Stmt.AssignCall("prop_val_lev_name", Expr.Val (Val.Str "propVal"), [e_f]);
    Stmt.FieldLookup("prop_val_lev", e_o, Expr.Var "prop_val_lev_name");
    Stmt.If (Expr.BinOpt (Oper.Leq, ctx, Expr.Var "prop_val_lev"), 
    Stmt.Block ([
      Stmt.FieldAssign (e_o, Expr.Var "prop_val_lev_name", (Expr.BinOpt (Oper.Lub, ctx, e_v_lev)));
      Stmt.FieldAssign (e_o, e_f, e_v)
    ]), Some (Stmt.Block ([
      Stmt.Exception ("Illegal Field Assign")
    ])))
    
  
   ]

and c_fielddelete (pc : string) (e_o : Expr.t) (e_f : Expr.t) : Stmt.t list =
  print_string "FieldDelete";
  let e_o_vars = Expr.vars e_o in
  let e_f_vars = Expr.vars e_f in
  let e_o_lev = level_list e_o_vars in
  let e_f_lev = level_list e_f_vars in
  let ctx1 = Expr.BinOpt (Oper.Leq, e_o_lev, e_f_lev) in
  let ctx = Expr.BinOpt (Oper.Leq, Expr.Var pc, ctx1) in
  [
   Stmt.AssignCall ("prop_exists_lev_name", Expr.Val (Val.Str "propExists"), [e_f]);
   Stmt.FieldLookup ("prop_exists_lev", e_o, Expr.Var "prop_exists_lev_name");
   Stmt.If ((Expr.BinOpt (Oper.Leq, ctx, Expr.Var "prop_exists_lev")), 
   Stmt.Block ([
    Stmt.FieldDelete (e_o, e_f)
   ]),
   Some (Stmt.Block ([
    Stmt.Exception ("Illegal Field Delete")
   ])))

  ]

and c_assinginobjcheck (pc : string) (st : string) (e_f : Expr.t) (e_o : Expr.t) : Stmt.t list =
  print_string "AssignInObjCheck";
  let e_o_vars = Expr.vars e_o in
  let e_f_vars = Expr.vars e_f in
  let e_o_lev = level_list e_o_vars in
  let e_f_lev = level_list e_f_vars in
  let ctx1 = Expr.BinOpt (Oper.Leq, e_o_lev, e_f_lev) in
  let ctx = Expr.BinOpt (Oper.Leq, Expr.Var pc, ctx1) in
  [
   Stmt.FieldLookup ("field_res_aux", e_o, e_f);
   Stmt.If (Expr.BinOpt (Oper.Leq, ctx, Val.shadowvar st), 
   Stmt.Block ([
      
   ]), Some (Stmt.Block ([

   ]))) 

  ]




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

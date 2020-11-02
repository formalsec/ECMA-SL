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
  	| Block lst -> []

    | Skip ->  [Stmt.Skip]

    | Assign (x,e) -> let vars = Expr.vars e in
                    let cond = Expr.BinOpt (Oper.Leq , (Expr.Var  (Val.shadowvar x)),  (Expr.Var pc) )in (*PC <= x_lev*)
                    let fresh = fresh_var () in
                    let st1 = Stmt.Block ([Stmt.Assign((Val.shadowvar x),(Expr.BinOpt (Oper.Lub,  (Expr.Var pc), (level_list vars))))] @ [Stmt.Assign (x,e)]) in
                    let out= (Stmt.If (cond ,st1 ,Some (Stmt.Block ([Stmt.Exception "MONITOR BLOCK - pc bigger than x"])))) in
                    [out]


  (*  |Print (s,e) -> let vars = Expr.vars e in

                let cond = Expr.BinOp ( Expr.Leq ,Expr.Val (Val.Str s) ,Expr.BinOp (Expr.Lub ,(level_list vars),  (Expr.Var pc) )) in (*PC U e_lev <= low *)
                let out= Stmt.If (cond, ([Stmt.Out (s,e)]), ([Stmt.Exception "MONITOR BLOCK - Illegal Output"]))   in
                [out]
*)


    | Return e -> let vars = Expr.vars e in
                  [Stmt.Return (Expr.Pair (e,Expr.BinOpt (Oper.Lub ,(level_list vars),  (Expr.Var pc) )))]


    | If (e,s1,s2) ->
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



    | While (e,s) -> let vars = Expr.vars e in
                     let pc' = fresh_pc () in
                     let pc_value = Expr.BinOpt (Oper.Lub,  (Expr.Var pc) , (level_list vars)) in
                     let s'= translist pc' s in
                     let while_stmt= [Stmt.While (e, Stmt.Block s')]    in
                      [Stmt.Assign ( pc',pc_value)]@ while_stmt

    | AssignCall (x,f,args) ->
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



    |_ ->		raise(Except "Unknown Op")(*ERROR*)


and translist (pc:string) (_stmts: Stmt.t ) : Stmt.t list=
match _stmts with
Block stmts -> List.fold_left (fun ac s -> ac @ compile pc s) [] stmts


let compile_functions (prog : Prog.t) (out_file : string): Prog.t =
  print_string ("Transpiling Program with Inlined Monitor...\n---------- New Code ---------- \n\n\n");
  let new_prog = Prog.create [] in
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

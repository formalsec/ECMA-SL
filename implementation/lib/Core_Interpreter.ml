type return =
| Intermediate of (Callstack.t * Stmt.t list * Store.t * Heap.t)
| Finalv of Val.t option


let add_fields_to (obj : Object.t) (fes : (Field.t * Expr.t) list) (eval_e : (Expr.t -> Val.t)) : unit =
  List.iter (fun (f, e) -> let e' = eval_e e in Object.set obj f e') fes

(*EDIT TO: Op.ml*)
let eval_inobj_expr (heap : Heap.t) (field : Val.t) (loc : Val.t) : Val.t =
  let b = match loc, field with
    | Loc l, Str f -> Heap.get_field heap l f
    | _            -> invalid_arg "Exception in Interpreter.eval_inobj_expr : \"loc\" is not a Loc value or \"field\" is not a string" in
  match b with
  | Some v -> Bool (true)
  | None -> Bool (false)

(*EDIT TO: Op.ml*)
let eval_unop (op : Expr.uopt) (v : Val.t) : Val.t =
  match op with
  | Neg    -> Oper.neg v
  | Not    -> Oper.not v
  | Typeof -> Oper.typeof v

(*EDIT TO: Op.ml*)
let eval_binopt_expr (heap : Heap.t) (op : Expr.bopt) (v1 : Val.t) (v2 : Val.t) : Val.t =
  match op with
  | Plus    -> Oper.plus (v1, v2)
  | Minus   -> Oper.minus (v1, v2)
  | Times   -> Oper.times (v1, v2)
  | Div     -> Oper.div (v1, v2)
  | Equal   -> Oper.equal (v1, v2)
  | Gt      -> Oper.gt (v1, v2)
  | Lt      -> Oper.lt (v1, v2)
  | Egt     -> Oper.egt (v1, v2)
  | Elt     -> Oper.elt (v1, v2)
  | Log_And -> Oper.log_and (v1, v2)
  | Log_Or  -> Oper.log_or (v1, v2)
  | InObj   -> eval_inobj_expr heap v1 v2
(*EDIT TO: Op.ml*)
let eval_nopt_expr (op : Expr.nopt) (vals : Val.t list) : Val.t =
  match op with
  | ListExpr -> Val.List vals


let rec eval_expr (prog : Prog.t) (sto : Store.t) (e : Expr.t) : Val.t =
  match e with
  | Val n                -> n
  | Var x                -> Store.get sto x
  | UnOpt (uop, e)       -> let v = eval_expr prog heap sto e in eval_unop uop v
  | BinOpt (bop, e1, e2) -> let v1 = eval_expr prog heap sto e1 and v2 = eval_expr prog heap sto e2 in eval_binopt_expr heap bop v1 v2
  | NOpt (nop, es)       -> eval_nopt_expr nop (List.map (eval_expr prog heap sto) es)
  | NewObj (fes)         -> let obj = Object.create() in add_fields_to obj fes (eval_expr prog heap sto); Loc (Heap.insert heap obj)
  | Access (e, f)        -> eval_access_expr prog heap sto e f


(* Syntax for mutually recursive functions *)
and eval_call_expr (prog : Prog.t) (cs:Callstack.t) (heap : Heap.t) (sto : Store.t) (f_name : Expr.t) (es : Expr.t list) : Val.t =
  let vs = List.map (eval_expr prog heap sto) es in
  try
    let fname = eval_expr prog heap sto f_name in
    match fname with
    | Str f -> fst (eval_proc prog cs heap f vs)
    | _     -> invalid_arg "Exception in Interpreter.eval_call_expr | value found in store is not a string."
  with Not_found -> fst (eval_proc prog cs heap (Expr.str f_name) vs)


and eval_access_expr (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e : Expr.t) (f : Expr.t) : Val.t =
  let loc = eval_expr prog heap sto e in
  let field = eval_expr prog heap sto f in
  let loc' = (match loc with
      | Loc loc -> loc
      | _       -> invalid_arg "Exception in Interpreter.eval_access_expr : \"e\" didn't evaluate to Loc") in
  let field' = (match field with
      | Str field -> field
      | _         -> invalid_arg "Exception in Interpreter.eval_access_expr : \"f\" didn't evaluate to Str") in
  let v = Heap.get_field heap loc' field' in
  match v with
  | None    -> Undef
  | Some v' -> v'

                 (*
and eval_stmt (prog : Prog.t) (cs:Callstack.t) (heap : Heap.t) (sto: Store.t) (s: Stmt.t) : Val.t option = match s with
    Skip                      -> None
  | Assign (x, e)             -> let v = eval_expr prog heap sto e in Store.set sto x v; None
  | Seq (s1, s2)              -> (let v1 = eval_stmt prog cs heap sto s1 in
                                  match v1 with
                                  | None -> eval_stmt prog cs heap sto s2
                                  | Some v1 -> Some v1)
  | If (exps_stmts)           -> eval_if_stmt prog cs heap sto exps_stmts
  | While (e, s)              -> eval_stmt prog cs heap sto (If ([Some e, Seq (s, While (e, s))]))
  | Return exp                -> let v = eval_expr prog heap sto exp in Some v
  | FieldAssign (e_o, f, e_v) -> eval_fieldassign_stmt prog heap sto e_o f e_v
  | FieldDelete (e, f)        -> eval_fielddelete_stmt prog heap sto e f

and eval_if_stmt (prog : Prog.t) (cs : Callstack.t) (heap : Heap.t) (sto: Store.t) (exps_stmts : (Expr.t option * Stmt.t) list) : Val.t option =
  if List.length exps_stmts = 0 then None
  else let e_s = List.hd exps_stmts and rest = List.tl exps_stmts in
    match e_s with
    | Some e, s -> (let v = eval_expr prog heap sto e in
                    if (Val.is_true v) then eval_stmt prog cs heap sto s
                    else eval_stmt prog cs heap sto (If rest))
    | None, s   -> eval_stmt prog cs heap sto s
                   *)

and eval_fielddelete_stmt (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e : Expr.t) (f : Expr.t) =
  let loc = eval_expr prog heap sto e and field = eval_expr prog heap sto f in
  let loc' = (match loc with
      | Loc loc -> loc
      | _ -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc value") in
  let field' = (match field with
      | Str field -> field
      | _         -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"f\" didn't evaluate to Str") in
  Heap.delete_field heap loc' field'; None

and eval_fieldassign_stmt (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e_o : Expr.t) (f : Expr.t) (e_v : Expr.t) : Val.t option =
  let loc = eval_expr prog heap sto e_o and field = eval_expr prog heap sto f and v = eval_expr prog heap sto e_v in
  let loc' = (match loc with
      | Loc loc -> loc
      | _       -> invalid_arg "Exception in Interpreter.eval_fieldassign_stmt : \"e_o\" is not a Loc value") in
  let field' = (match field with
      | Str field -> field
      | _         -> invalid_arg "Exception in Interpreter.eval_fieldassign_stmt : \"f\" didn't evaluate to Str") in
  Heap.set_field heap loc' field' v; None

and eval_proc (prog : Prog.t) (cs:Callstack.t) (heap : Heap.t) (pname : string) (args : Val.t list) : Val.t * Store.t =
  let func = Prog.get_func prog pname in
  let sto = Func.create_store func args in
  let s = Func.get_body func in
  let v0 = eval_stmt prog cs heap sto s in
  match v0 with
    None -> raise (Invalid_argument "Exception in Interpreter.eval_proc: statement didn't return")
  | Some v -> (v, sto)



(*============================================================*)

let eval_small_step (prog: Prog.t) (cs: Callstack.t)  (sto: Store.t) (cont: Stmt.t list) (mode:string) (verbose: bool) (s: Stmt.t) : return  =

  match s with
  | Skip ->  Intermediate (cs, cont,sto, heap)

  | Asgn (x,e) -> (let v = eval_expr prog sto e in
                  Store.set_store sto x v;
                  print_string ("STORE: " ^ (x) ^ " <- " ^   Val.str v ^"\n");
                  Intermediate (cs, cont, sto, heap))




  | Return e -> let v = eval_expr prog sto e in
                let  (f,cs') = Callstack.pop cs in (
                match f with
                | Callstack.Intermediate (cont',sto', x, heap) ->   (Store.set_store sto'  (Val.str x) v;
                                                                                Intermediate (cs',cont',sto', heap))


                | Callstack.Toplevel -> Finalv (Some v))



  |If (e,s1,s2) -> let v = eval_expr prog sto e in
                       if (Val.is_true v) then Intermediate (cs,(s1 @ cont),sto, heap)
                       else match s2 with
                         | Some v -> Intermediate (cs,(s2 @ cont),sto, heap)
                         | None ->  Intermediate (cs,cont,sto, heap)


  | While (e,s) -> let (stms:Stmt.t list) = s @ (Stmt.While (e,s) :: []) in
                     Intermediate (cs, (Stmt.If (e, stms, None ) :: cont),sto, heap)

  | Call (x,f,es) -> let cs' = Callstack.push cs (Callstack.Intermediate (cont, sto, x)) in
                     let vs = (List.map (eval_expr prog sto) es) in
                     let pvs = List.combine (Prog.get_params prog f) vs in
                     let sto_aux = Store.create_store pvs in
                     let func = (Prog.get_func prog f) in
                     let (cont':Stmt.t list) = func.body in
                     Intermediate (cs', cont', sto_aux, heap)

  | FieldAssign (e_o, f, e_v) -> eval_fieldassign_stmt prog heap sto e_o f e_v;
                                 Intermediate (cs, cont, sto_aux, heap)
  | FieldDelete (e, f)        -> eval_fielddelete_stmt prog heap sto e f;
                                 Intermediate (cs, cont, sto_aux, heap)
  | AssignCall   (st, ef, el)->

  | AssignNewObj (st) ->let newobj= Object.create in
                        let loc= heap.insert heap obj in
                        Store.set_store sto st (Val.Loc loc);
                        print_string ("STORE: " ^ st ^ " <- " ^   Val.str v ^"\n");
                        Intermediate (cs, cont, sto, heap)


  | AssignAccess (st, ef, ep) -> let loc= eval_expr prog sto ef in
                                 let field = eval_expr prog heap sto ep in
                                 let loc' = (match loc with
                                      | Loc loc -> loc
                                      | _       -> invalid_arg "Exception in Interpreter.eval_access_expr : \"e\" didn't evaluate to Loc") in
                                 let field' = (match field with
                                      | Str field -> field
                                      | _         -> invalid_arg "Exception in Interpreter.eval_access_expr : \"f\" didn't evaluate to Str") in
                                 let v = Heap.get_field heap loc' field' in
                                 match v with
                                  | None    -> Val.Undef
                                  | Some v' -> v'
                                 Store.set_store sto st v';
                                 print_string ("STORE: " ^ st ^ " <- " ^   Val.str v' ^"\n");
                                 Intermediate (cs, cont, sto, heap)



  |_ ->   raise(Except "Unknown Op")(*ERROR*)

(*This function will iterate smallsteps in a list of functions*)
let rec small_step_iter (prog:Prog.t) (cs:Callstack.t) (heap:Heap.t) (sto:Store.t) (stmts:Stmt.t list)  (verbose:bool): return =
  match stmts with
    | [] ->  Intermediate (cs, stmts, sto) (*Raise(error??)*)
    | s::stmts' -> ( let return = eval_small_step prog cs sto stmts' verbose s in


          match return with
          |Finalv v ->  Finalv v
          |Intermediate (cs', stmts'', sto') -> small_step_iter prog cs' sto' stmts'' verbose)


(*Worker class of the Interpreter*)
let eval_prog (prog : Prog.t) ( cs: Callstack.t) (heap:Heap.t) (out:string) (verbose:bool) (main:string) : Val.t option =
  let sto = Store.create_store [] in
  let cs'= Callstack.push cs (Callstack.Toplevel) in
  let func = (Prog.get_func prog main(*passar como argumento valores e nome*)) in
  let v=  small_step_iter prog cs sto func.body verbose in
  match v with
  |Finalv v -> v
  | _ -> raise(Except "No return value")(*ERROR*)

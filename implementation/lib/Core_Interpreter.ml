exception Except of string
type return =
  | Intermediate of (Callstack.t * Stmt.t list * Store.t * Heap.t)
  | Finalv of Val.t option


let add_fields_to (obj : Object.t) (fes : (Field.t * Expr.t) list) (eval_e : (Expr.t -> Val.t)) : unit =
  List.iter (fun (f, e) -> let e' = eval_e e in Object.set obj f e') fes


let eval_inobj_expr (heap : Heap.t) (field : Val.t) (loc : Val.t) : Val.t =
  let b = match loc, field with
    | Loc l, Str f -> Heap.get_field heap l f
    | _            -> invalid_arg "Exception in Interpreter.eval_inobj_expr : \"loc\" is not a Loc value or \"field\" is not a string" in
  match b with
  | Some v -> Bool (true)
  | None -> Bool (false)


let eval_unop (op : Oper.uopt) (v : Val.t) : Val.t =
  match op with
  | Neg    -> Oper.neg v
  | Not    -> Oper.not v
  | Typeof -> Oper.typeof v


let eval_binopt_expr (op : Oper.bopt) (v1 : Val.t) (v2 : Val.t) : Val.t =
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
  | _ -> raise(Except "Not expected")


let eval_nopt_expr (op : Oper.nopt) (vals : Val.t list) : Val.t =
  match op with
  | ListExpr -> Val.List vals


let rec eval_expr (sto : Store.t) (e : Expr.t) : Val.t =
  match e with
  | Val n                -> n
  | Var x                -> Store.get sto x
  | UnOpt (uop, e)       -> let v = eval_expr sto e in
    eval_unop uop v
  | BinOpt (bop, e1, e2) -> let v1 = eval_expr sto e1
    and v2 = eval_expr sto e2 in
    eval_binopt_expr bop v1 v2
  | NOpt (nop, es)       -> eval_nopt_expr nop (List.map (eval_expr sto) es)

let get_func_id (sto:Store.t) (exp:Expr.t) :string=
  let res= eval_expr sto exp in
  match res with
  | Val.Str f -> f 
  | _ -> raise (Except "Wrong/Invalid Function ID")
  
let prepare_call  (prog:Prog.t) (cs:Callstack.t)(sto: Store.t) (cont: Stmt.t list) (x:string) (es:Expr.t list) (f:string): (Callstack.t * Store.t * Val.t list) =
  let cs' = Callstack.push cs (Callstack.Intermediate (cont, sto, x)) in
  let vs = (List.map (eval_expr sto) es) in
  let pvs = List.combine (Prog.get_params prog f) vs in
  let sto_aux = Store.create pvs in
  (cs', sto_aux, vs)

let eval_inobj_expr (prog: Prog.t) (heap : Heap.t) (sto : Store.t) (field : Expr.t) (loc : Expr.t) : Val.t =
  let loc' = eval_expr sto loc in
  let field' = eval_expr sto field in
  let b =  match loc', field' with
    | Loc l, Str f -> Heap.get_field heap l f
    | _            -> invalid_arg "Exception in Interpreter.eval_inobj_expr : \"loc\" is not a Loc value or \"field\" is not a string" in
  match b with
  | Some v -> Bool (true)
  | None -> Bool (false)


let eval_fielddelete_stmt (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e : Expr.t) (f : Expr.t): unit =
  let loc = eval_expr sto e and field = eval_expr sto f in
  let loc' = (match loc with
      | Loc loc -> loc
      | _ -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc value") in
  let field' = (match field with
      | Str field -> field
      | _         -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"f\" didn't evaluate to Str") in
  Heap.delete_field heap loc' field';

and eval_fieldassign_stmt (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e_o : Expr.t) (f : Expr.t) (e_v : Expr.t) : unit =
  let loc = eval_expr sto e_o and field = eval_expr sto f and v = eval_expr sto e_v in
  let loc' = (match loc with
      | Loc loc -> loc
      | _       -> invalid_arg "Exception in Interpreter.eval_fieldassign_stmt : \"e_o\" is not a Loc value") in
  let field' = (match field with
      | Str field -> field
      | _         -> invalid_arg "Exception in Interpreter.eval_fieldassign_stmt : \"f\" didn't evaluate to Str") in
  Heap.set_field heap loc' field' v


(* \/ ======================================= Main InterpreterFunctions ======================================= \/ *)


let rec eval_small_step (interceptor: string -> Val.t list -> SecLabel.t option) (prog: Prog.t) (cs: Callstack.t)  (heap:Heap.t) (sto: Store.t) (cont: Stmt.t list) (verbose: bool) (s: Stmt.t) : (return * SecLabel.t)  =
  print_string ("\n>>> "^Stmt.str s^"\n");
  match s with
  | Skip ->  (Intermediate (cs, cont,sto, heap), SecLabel.EmptyLab)

  | Assign (x,e) -> (let v = eval_expr sto e in
                     Store.set sto x v;
                     print_string ("STORE: " ^ (x) ^ " <- " ^   Val.str v ^"\n");
                     (Intermediate (cs, cont, sto, heap)), SecLabel.AsgnLab (x,e))




  | Return e -> let v = eval_expr sto e in
    let  (f,cs') = Callstack.pop cs in (
      match f with
      | Callstack.Intermediate (cont',sto', x) ->   (Store.set sto'  x v;
                                                     (Intermediate (cs',cont',sto', heap), SecLabel.RetLab e))


      | Callstack.Toplevel -> (Finalv (Some v), SecLabel.RetLab e))

  | Block block -> (Intermediate (cs,(block @ cont),sto,heap), SecLabel.EmptyLab )


  | If (e,s1,s2) -> let v = eval_expr sto e in
    if (Val.is_true v) then
      match s1 with
      | Block block -> (match s2 with
          |Some v -> Intermediate (cs,(block @ cont),sto, heap), SecLabel.BranchLab (e,v)
          |None -> Intermediate (cs,(block @ cont),sto, heap), SecLabel.BranchLab (e,(Stmt.Skip)))
      | _ -> raise (Except "IF block expected ")

    else
      (match s2 with
       | Some v -> (match v with
           | Block block2 -> Intermediate (cs, (block2 @ cont), sto, heap),SecLabel.BranchLab (e,s1)
           |_ -> raise (Except "Not expected"))
       | None ->  (Intermediate (cs,cont,sto, heap), SecLabel.EmptyLab))



  | While (e,s) ->let s1 = (s :: []) @ (Stmt.While (e,s) :: []) in
    let stms= Stmt.If (e, (Stmt.Block s1), None) in
    (Intermediate (cs, (stms :: cont),sto, heap), SecLabel.EmptyLab)

  | AssignCall (x,f,es) -> 
    let f'= get_func_id sto f in 
    let (cs', sto_aux, vs) = prepare_call  prog cs sto cont x es f' in
    let func = (Prog.get_func prog f') in
    let b = interceptor f' vs in
   ( match b with
     |None -> (let (cont':Stmt.t) = func.body in
               let aux_list= (cont'::[]) in
              (Intermediate (cs', aux_list, sto_aux, heap), SecLabel.CallLab (es,x,(Expr.str f))))
     |Some lab -> (Intermediate(cs,cont,sto,heap),lab))

  |AssignInObjCheck (st, e1, e2) ->
    let v= eval_inobj_expr prog heap sto e1 e2 in
    Store.set sto st v;
    (Intermediate (cs, cont, sto, heap), SecLabel.AsgnLab (st,e1))


  | AssignNewObj (st) ->let newobj= Object.create () in
    let loc= Heap.insert heap newobj in
    Store.set sto st (Val.Loc loc);
    print_string ("STORE: " ^ st ^ " <- " ^   Val.str (Val.Loc loc) ^"\n");
    (Intermediate (cs, cont, sto, heap), SecLabel.EmptyLab)

  | AssignAccess (st, ef, ep) -> let loc= eval_expr sto ef in
    let field = eval_expr sto ep in
    (match loc,field with
    | Loc loc', Str field' -> (let v = Heap.get_field heap loc' field' in
                               let v' =(match v with
                                   | None    -> Val.Undef
                                   | Some v'' -> v''
                                 ) in
                               Store.set sto st v';
                               print_string ("STORE: " ^ st ^ " <- " ^   Val.str v' ^"\n");
                               (Intermediate (cs, cont, sto, heap), SecLabel.AsgnLab (st,ep)))
    | _       -> invalid_arg "Exception in Interpreter.eval_access_expr : \"e\" didn't evaluate to Loc"
    )


    | FieldAssign (e_o, f, e_v) -> eval_fieldassign_stmt prog heap sto e_o f e_v;
      (Intermediate (cs, cont, sto, heap), SecLabel.AsgnLab ((Expr.str f),e_v))

    | FieldDelete (e, f)        -> eval_fielddelete_stmt prog heap sto e f;
      (Intermediate (cs, cont, sto, heap), SecLabel.AsgnLab ((Expr.str f),e))




    | _ ->   raise(Except "Unknown Op")(*ERROR*)

(*This function will iterate smallsteps in a list of functions*)
and  small_step_iter (interceptor: string -> Val.t list -> SecLabel.t option) (prog:Prog.t) (cs:Callstack.t) (heap:Heap.t) (sto:Store.t) (stmts:Stmt.t list)  (verbose:bool): return =
  print_string "small_iter";
  match stmts with
  | [] ->  raise(Except "Empty list")
  | s::stmts' -> ( let (return, label) = eval_small_step interceptor prog cs heap sto stmts' verbose s in


                   match return with
                   |Finalv v ->  Finalv v
                   |Intermediate (cs', stmts'', sto',heap') -> small_step_iter interceptor prog cs' heap' sto' stmts'' verbose)


(*Worker class of the Interpreter*)
let eval_prog (interceptor: string -> Val.t list -> SecLabel.t option) (prog : Prog.t) ( cs: Callstack.t) (heap:Heap.t) (out:string) (verbose:bool) (main:string) : Val.t option =
  let sto = Store.create [] in
  let cs'= Callstack.push cs (Callstack.Toplevel) in
  let func = (Prog.get_func prog main(*passar como argumento valores e nome*)) in
  let v = small_step_iter interceptor prog cs' heap sto [func.body] verbose in
  (*let v=  small_step_iter prog cs heap sto func.body verbose in*)
  match v with
  |Finalv v -> v
  | _ -> raise(Except "No return value")(*ERROR*)

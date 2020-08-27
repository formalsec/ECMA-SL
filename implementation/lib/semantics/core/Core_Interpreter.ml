exception Except of string

type state_t = Callstack.t * Heap.t * Store.t

type return =
  | Intermediate of state_t * Stmt.t list
  (*| Intermediate of (Callstack.t * Stmt.t list * Store.t * Heap.t)*)
  | Finalv of Val.t option


let add_fields_to (obj : Object.t) (fes : (Field.t * Expr.t) list) (eval_e : (Expr.t -> Val.t)) : unit =
  List.iter (fun (f, e) -> let e' = eval_e e in Object.set obj f e') fes


let eval_unop (op : Oper.uopt) (v : Val.t) : Val.t =
  match op with
  | Neg      -> Oper.neg v
  | Not      -> Oper.not v
  | Typeof   -> Oper.typeof v
  | ListLen  -> Oper.l_len v
  | TupleLen -> Oper.t_len v
  | Head     -> Oper.head v
  | Tail     -> Oper.tail v
  | First    -> Oper.first v
  | Second   -> Oper.second v


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
  | Lnth    -> Oper.list_nth (v1, v2)
  | Tnth    -> Oper.tuple_nth (v1, v2)
  | Ladd    -> Oper.list_add (v1, v2)
  | InObj   -> raise(Except "Not expected")


let eval_nopt_expr (op : Oper.nopt) (vals : Val.t list) : Val.t =
  match op with
  | ListExpr  -> Val.List vals
  | TupleExpr -> Val.Tuple vals
  | NAry_And  -> Val.Bool (List.for_all Oper.is_true vals)
  | NAry_Or   -> Val.Bool (List.exists Oper.is_true vals)


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

let prepare_call  (prog:Prog.t) (cs:Callstack.t)(sto: Store.t) (cont: Stmt.t list) (x:string) (es:Expr.t list) (f:string): (Callstack.t * Store.t * Val.t list * string list) =
  let cs' = Callstack.push cs (Callstack.Intermediate (cont, sto, x)) in
  let vs = (List.map (eval_expr sto) es) in
  let params = Prog.get_params prog f in
  let pvs = List.combine params vs in
  let sto_aux = Store.create pvs in
  (cs', sto_aux, vs, params)

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


let eval_small_step (interceptor: string -> Val.t list -> Expr.t list -> SecLabel.t option) (prog: Prog.t) (state : state_t) (cont: Stmt.t list) (verbose: bool) (s: Stmt.t) : (return * SecLabel.t)  =
  print_string ("\n>>> "^Stmt.str s^"\n");
  let (cs, heap, sto)= state in
  match s with
  | Skip ->
    (Intermediate ((cs, heap, sto), cont), SecLabel.EmptyLab)

  | Print e ->
    (let v = eval_expr sto e in
     print_endline ("PROGRAM PRINT: " ^ (Val.str v));
     (Intermediate ((cs, heap, sto), cont), SecLabel.EmptyLab)
    )

  | Assign (x,e) ->
    (let v = eval_expr sto e in
     Store.set sto x v;
     print_string ("STORE: " ^ (x) ^ " <- " ^   Val.str v ^"\n");
     (Intermediate ((cs, heap, sto), cont), SecLabel.AssignLab (x,e)))


  | Return e ->
    let v = eval_expr sto e in
    let  (f,cs') = Callstack.pop cs in (
      match f with
      | Callstack.Intermediate (cont',sto', x) ->   (Store.set sto'  x v;
                                                     (Intermediate ((cs', heap, sto'), cont'), SecLabel.ReturnLab e))


      | Callstack.Toplevel -> (Finalv (Some v), SecLabel.ReturnLab e))

  | Block block ->
    (Intermediate ((cs, heap, sto), (block @ cont)), SecLabel.EmptyLab )



  | If (e,s1,s2) ->
    let v = eval_expr sto e in
    if (Oper.is_true v) then
      match s1 with
      | Block block ->
        (match s2 with
         |Some v -> Intermediate ((cs, heap, sto), (block @ cont)), SecLabel.BranchLab (e,v)
         |None -> Intermediate ((cs, heap, sto), (block @ cont)), SecLabel.BranchLab (e,(Stmt.Skip)))
      | _ -> raise (Except "IF block expected ")

    else
      (match s2 with
       | Some v ->
         (match v with
          | Block block2 -> Intermediate ((cs, heap, sto), (block2 @ cont)),SecLabel.BranchLab (e,s1)
          |_ -> raise (Except "Not expected"))
       | None ->
         (Intermediate ((cs, heap, sto), cont), SecLabel.EmptyLab))



  | While (e,s) ->
    let s1 = (s :: []) @ (Stmt.While (e,s) :: []) in
    let stms= Stmt.If (e, (Stmt.Block s1), None) in
    (Intermediate ((cs, heap, sto), (stms :: cont)), SecLabel.EmptyLab)

  | AssignCall (x,f,es) ->
    let f'= get_func_id sto f in
    let (cs', sto_aux, vs, params) = prepare_call  prog cs sto cont x es f' in
    let func = (Prog.get_func prog f') in
    let b = interceptor f' vs es in
    ( match b with
      |None ->
        (let (cont':Stmt.t) = func.body in
         let aux_list= (cont'::[]) in
         (Intermediate ((cs', heap, sto_aux), aux_list), SecLabel.AssignCallLab (params, es, x, f')))
      |Some lab ->
        (Intermediate((cs, heap, sto), cont),lab))

  | AssignInObjCheck (st, e1, e2) ->
    let v= eval_inobj_expr prog heap sto e1 e2 in
    Store.set sto st v;
    (Intermediate ((cs, heap, sto), cont), SecLabel.AssignLab (st,e1))


  | AssignNewObj (st) ->
    let newobj= Object.create () in
    let loc= Heap.insert heap newobj in
    Store.set sto st (Val.Loc loc);
    print_string ("STORE: " ^ st ^ " <- " ^   Val.str (Val.Loc loc) ^"\n");
    (Intermediate ((cs, heap, sto), cont), SecLabel.EmptyLab)


  | FieldLookup (x, e_o, e_f) ->
    let loc= eval_expr sto e_o in
    let field = eval_expr sto e_f in
    (match loc,field with
     | Loc loc', Str field' ->
       (let v = Heap.get_field heap loc' field' in
        let v' =(match v with
            | None     -> Val.Symbol "'undefined"
            | Some v'' -> v''
          ) in
        Store.set sto x v';
        print_string ("STORE: " ^ x ^ " <- " ^   Val.str v' ^"\n");
        (Intermediate ((cs, heap, sto), cont), SecLabel.FieldLookupLab (x, loc', field', e_o, e_f)))
     | _                    ->
       invalid_arg ("Exception in Interpreter.eval_access_expr : \"e\" didn't evaluate to Loc."))


  | FieldAssign (e_o, f, e_v) ->
    eval_fieldassign_stmt prog heap sto e_o f e_v;
    (Intermediate ((cs, heap, sto), cont), SecLabel.EmptyLab) (*TODO*)

  | FieldDelete (e, f)        ->
    let loc = eval_expr sto e in
    let field = eval_expr sto f in
    (match loc, field with
     | Loc loc', Str field' ->
       Heap.delete_field heap loc' field';
       (Intermediate ((cs, heap, sto), cont), SecLabel.FieldDeleteLab (loc', field', e, f))
     | _ -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc value")






(*This function will iterate smallsteps in a list of functions*)
let rec  small_step_iter (interceptor: string -> Val.t list -> Expr.t list  -> SecLabel.t option) (prog:Prog.t) (state : state_t) (mon_state:NSU_Monitor.monitor_state_t) (stmts:Stmt.t list)  (verbose:bool): return =
  print_string "small_iter";
  match stmts with
  | [] ->  raise(Except "Empty list")
  | s::stmts' -> ( let (return, label) = eval_small_step interceptor prog state stmts' verbose s in
                   let mon_return : NSU_Monitor.monitor_return = NSU_Monitor.eval_small_step mon_state label in
                   (match mon_return with
                    | MReturn mon_state' -> (
                        match return with
                        |Finalv v ->  Finalv v
                        |Intermediate (state', stmts'') ->
                          small_step_iter interceptor prog state' mon_state' stmts'' verbose)
                    | MFail  (mon_state', str) ->
                      print_string ("\nX X X X X X X X X X X X\nMONITOR EXCEPTION -> "^str);
                      raise (Except "")))


let initial_state () : state_t =
  let sto = Store.create [] in
  let cs = Callstack.push [] (Callstack.Toplevel) in
  let heap = Heap.create () in
  (cs, heap, sto)

(*Worker class of the Interpreter*)
let eval_prog (interceptor: string -> Val.t list -> Expr.t list -> SecLabel.t option) (prog : Prog.t) (out:string) (verbose:bool) (main:string) : Val.t option =
  let func = (Prog.get_func prog main(*passar como argumento valores e nome*)) in
  let state_0 = initial_state () in
  let mon_state_0 = NSU_Monitor.initial_monitor_state () in
  let v = small_step_iter interceptor prog state_0 mon_state_0 [func.body] verbose in
  (*let v=  small_step_iter prog cs heap sto func.body verbose in*)
  match v with
  |Finalv v -> v
  | _ -> raise(Except "No return value")(*ERROR*)

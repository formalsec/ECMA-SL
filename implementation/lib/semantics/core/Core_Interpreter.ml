module M 
  (Mon : SecurityMonitor.M) = struct 


exception Except of string

type state_t = Callstack.t * Heap.t * Store.t

type return =
  | Intermediate of state_t * Stmt.t list
  | Finalv of Val.t option

type ctx_t = string * string * bool


let add_fields_to (obj : Object.t) (fes : (Field.t * Expr.t) list) (eval_e : (Expr.t -> Val.t)) : unit =
  List.iter (fun (f, e) -> let e' = eval_e e in Object.set obj f e') fes


let eval_unop (op : Oper.uopt) (v : Val.t) : Val.t =
  match op with
  | Neg           -> Oper.neg v
  | Not           -> Oper.not v
  | Typeof        -> Oper.typeof v
  | ListLen       -> Oper.l_len v
  | TupleLen      -> Oper.t_len v
  | Head          -> Oper.head v
  | Tail          -> Oper.tail v
  | First         -> Oper.first v
  | Second        -> Oper.second v
  | IntToFloat    -> Oper.int_to_float v
  | IntToString   -> Oper.int_to_string v
  | IntOfString   -> Oper.int_of_string v
  | FloatToString -> Oper.float_to_string v
  | Sconcat       -> Oper.string_concat v
  | ObjToList     -> raise (Failure "Unexpected call to Core_Interpreter.eval_unop with operator ObjToList")
  | ObjFields     -> raise (Failure "Unexpected call to Core_Interpreter.eval_unop with operator ObjFields")
  | ToUint32      -> Oper.to_uint32 v


let eval_binopt_expr (op : Oper.bopt) (v1 : Val.t) (v2 : Val.t) : Val.t =
  match op with
  | Plus     -> Oper.plus (v1, v2)
  | Minus    -> Oper.minus (v1, v2)
  | Times    -> Oper.times (v1, v2)
  | Div      -> Oper.div (v1, v2)
  | Equal    -> Oper.equal (v1, v2)
  | Gt       -> Oper.gt (v1, v2)
  | Lt       -> Oper.lt (v1, v2)
  | Egt      -> Oper.egt (v1, v2)
  | Elt      -> Oper.elt (v1, v2)
  | Log_And  -> Oper.log_and (v1, v2)
  | Log_Or   -> Oper.log_or (v1, v2)
  | Lnth     -> Oper.list_nth (v1, v2)
  | Tnth     -> Oper.tuple_nth (v1, v2)
  | Ladd     -> Oper.list_add (v1, v2)
  | Lprepend -> Oper.list_prepend (v1, v2)
  | Lconcat  -> Oper.list_concat (v1, v2)
  | InList   -> Oper.list_in (v1, v2)
  | InObj    -> raise(Except "Not expected")


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
  | BinOpt (bop, e1, e2) -> 
    let v1 = eval_expr sto e1 in 
    let v2 = eval_expr sto e2 in
    eval_binopt_expr bop v1 v2
  | NOpt (nop, es)       -> eval_nopt_expr nop (List.map (eval_expr sto) es)

let get_func_id (sto:Store.t) (exp:Expr.t) :string=
  let res= eval_expr sto exp in
  match res with
  | Val.Str f -> f
  | _ -> raise (Except "Wrong/Invalid Function ID")

let prepare_call  (prog:Prog.t) (cs:Callstack.t)(sto: Store.t) (cont: Stmt.t list) (x:string) (es:Expr.t list) (f:string) (vs: Val.t list): (Callstack.t * Store.t * string list) =
  let cs' = Callstack.push cs (Callstack.Intermediate (cont, sto, x)) in
  let params = Prog.get_params prog f in
  let pvs = List.combine params vs in
  let sto_aux = Store.create pvs in
  (cs', sto_aux, params)

let eval_inobj_expr (prog: Prog.t) (heap : Heap.t) (sto : Store.t) (field : Val.t) (loc : Val.t) : Val.t =
  let b =  match loc, field with
    | Loc l, Str f -> Heap.get_field heap l f
    | _            -> invalid_arg "Exception in Interpreter.eval_inobj_expr : \"loc\" is not a Loc value or \"field\" is not a string" in
  match b with
  | Some v -> Bool (true)
  | None -> Bool (false)

let eval_objtolist_oper (heap : Heap.t) (st : Store.t) (loc_expr : Expr.t) : Val.t =
  let loc = eval_expr st loc_expr in
  let loc' = (match loc with
      | Loc l -> l
      | _     -> invalid_arg "Exception in Core_Interpreter.eval_objtolist_oper: \"loc\" is not a Loc value") in
  let obj = Heap.get heap loc' in
  match obj with
  | None   -> invalid_arg ("Exception in Core_Interpreter.eval_objtolist_oper: \"" ^ Loc.str loc' ^ "\" doesn't exist in the Heap")
  | Some o -> let fvs = Object.to_list o in (List (List.map (fun (f, v) -> Val.Tuple (Str f :: [v])) fvs))

let eval_objfields_oper (heap : Heap.t) (st : Store.t) (loc_expr : Expr.t) : Val.t =
  let loc = eval_expr st loc_expr in
  let loc' = (match loc with
      | Loc l -> l
      | _     -> invalid_arg "Exception in Core_Interpreter.eval_objfields_oper: \"loc\" is not a Loc value") in
  let obj = Heap.get heap loc' in
  match obj with
  | None   -> invalid_arg ("Exception in Core_Interpreter.eval_objfields_oper: \"" ^ Loc.str loc' ^ "\" doesn't exist in the Heap")
  | Some o -> List (List.map (fun f -> Val.Str f) (Object.get_fields o))

let eval_fielddelete_stmt (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e : Expr.t) (f : Expr.t): unit =
  let loc = eval_expr sto e and field = eval_expr sto f in
  let loc' = (match loc with
      | Loc loc -> loc
      | _ -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc value") in
  let field' = (match field with
      | Str field -> field
      | _         -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"f\" didn't evaluate to Str") in
  Heap.delete_field heap loc' field'

  let str_sset_to_list (str : string) : string list =
    let rem1 = String.split_on_char '{' str in
    let rem2 = String.split_on_char '}' (List.nth rem1 1) in
    let finalst = String.split_on_char ',' (List.nth rem2 0) in
      finalst




(* \/ ======================================= Main InterpreterFunctions ======================================= \/ *)


let eval_small_step (interceptor: string -> Val.t list -> Expr.t list -> (Mon.sl SecLabel.t) option) (prog: Prog.t) (state : state_t) (cont: Stmt.t list) (verbose: bool) (s: Stmt.t) : (return * (Mon.sl SecLabel.t))  =
  let (cs, heap, sto)= state in
  print_string ("\n====================================\nEvaluating >>>>> "^(Stmt.str s) ^ "\n");
  match s with
  | Skip ->
    (Intermediate ((cs, heap, sto), cont), SecLabel.EmptyLab)
  
  | Merge -> (Intermediate ((cs, heap, sto), cont), SecLabel.MergeLab)

  | Print e ->
    (let v = eval_expr sto e in
    (match v with
    | Loc l ->
      (match Heap.get heap l with
        | Some o -> print_endline ("PROGRAM PRINT: " ^ Object.str o)
        | None   -> print_endline "PROGRAM PRINT: Non-existent location" )
    | _     -> print_endline ("PROGRAM PRINT: " ^ (Val.str v)));
     (Intermediate ((cs, heap, sto), cont), SecLabel.PrintLab (e))
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
        let blockm = (block @ (Stmt.Merge :: [])) in
        (match s2 with
         |Some v -> Intermediate ((cs, heap, sto), (blockm @ cont)), SecLabel.BranchLab (e,v)
         |None -> Intermediate ((cs, heap, sto), (blockm @ cont)), SecLabel.BranchLab (e,(Stmt.Skip)))
      | _ -> raise (Except "IF block expected ")

    else
      (match s2 with
       | Some v ->
         (match v with
          | Block block2 -> 
          let block2m = (block2 @ (Stmt.Merge :: [])) in
          Intermediate ((cs, heap, sto), (block2m  @ cont)), SecLabel.BranchLab (e,s1)
          |_ -> raise (Except "Not expected"))
       | None ->
         (Intermediate ((cs, heap, sto), cont), SecLabel.EmptyLab))



  | While (e,s) ->
    let s1 = (s :: []) @ (Stmt.While (e,s) :: []) in
    let stms= Stmt.If (e, (Stmt.Block s1), None) in
    (Intermediate ((cs, heap, sto), (stms :: cont)), SecLabel.EmptyLab)

  | AssignCall (x,f,es) ->
    let f'= get_func_id sto f in
    let vs = (List.map (eval_expr sto) es) in
    let b = interceptor f' vs es in
    ( match b with
      |None ->
        let func = (Prog.get_func prog f') in
        let (cs', sto_aux, params) = prepare_call prog cs sto cont x es f' vs in
        (let (cont':Stmt.t) = func.body in
         let aux_list= (cont'::[]) in
         (Intermediate ((cs', heap, sto_aux), aux_list), SecLabel.AssignCallLab (params, es, x, f')))
      |Some lab ->
        (Intermediate((cs, heap, sto), cont),lab))

  | AssignInObjCheck (st, e_f, e_o) ->
    let f= eval_expr sto e_f in
    let o= eval_expr sto e_o in
    let f', o'= 
      match f, o with
      | Str f ,Loc o -> f, o
      | _ -> raise (Except "Internal Error") in
    let v= eval_inobj_expr prog heap sto f o in
    Store.set sto st v;
    (Intermediate ((cs, heap, sto), cont), SecLabel.AssignInObjCheckLab (st, f', o', e_f, e_o))


  | AssignNewObj (x) ->
    let newobj= Object.create () in
    let loc= Heap.insert heap newobj in
    Store.set sto x (Val.Loc loc);
    print_string ("STORE: " ^ x ^ " <- " ^   Val.str (Val.Loc loc) ^"\n");
    (Intermediate ((cs, heap, sto), cont), SecLabel.NewLab (x, loc))


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


  | FieldAssign (e_o, e_f, e_v) ->
    let loc = eval_expr sto e_o in
    let field = eval_expr sto e_f in
    let v = eval_expr sto e_v in
    (match loc, field with
     | Loc loc , Str field ->
       Heap.set_field heap loc field v;
       (Intermediate ((cs, heap, sto), cont), SecLabel.FieldAssignLab (loc,field, e_o, e_f, e_v))
     | _       ->
       invalid_arg "Exception in Interpreter.eval_fieldassign_stmt : \"e_o\" is not a Loc value")



  | FieldDelete (e, f)        ->
    let loc = eval_expr sto e in
    let field = eval_expr sto f in
    (match loc, field with
     | Loc loc', Str field' ->
       Heap.delete_field heap loc' field';
       (Intermediate ((cs, heap, sto), cont), SecLabel.FieldDeleteLab (loc', field', e, f))
     | _ -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc value")


  | AssignObjToList (st, e) ->
    let v = eval_objtolist_oper heap sto e in
    Store.set sto st v;
    (Intermediate ((cs, heap, sto), cont), SecLabel.AssignLab (st, e))


  | AssignObjFields (st, e) ->
    let v = eval_objfields_oper heap sto e in
    Store.set sto st v;
    (Intermediate ((cs, heap, sto), cont), SecLabel.AssignLab (st, e))

  | SetTop exp -> 
    let st = eval_expr sto exp in
      (match st with 
      | Str v -> 
        let finalst = str_sset_to_list v in
        (Intermediate ((cs, heap, sto), cont), SecLabel.SetTopLab  finalst)
      | _ -> raise (Except "SetTop -> not a string"))

  | AllowFlow (exp1, exp2) -> 
    print_string "ALLOWFLOW\n";
    let st1 = eval_expr sto exp1 in
    let st2 = eval_expr sto exp2 in
    (match st1, st2 with
    | Str v1, Str v2 -> 
      let final1 = str_sset_to_list v1 in
      let final2 = str_sset_to_list v2 in
      (Intermediate ((cs, heap, sto), cont), SecLabel.AllowFlowLab  (final1, final2))
    | _, _ -> raise (Except "AllowFlow -> not a string"))
    

(*This function will iterate smallsteps in a list of functions*)
let rec  small_step_iter (interceptor: string -> Val.t list -> Expr.t list  -> (Mon.sl SecLabel.t) option) (prog:Prog.t) (state : state_t) (mon_state:Mon.state_t) (stmts:Stmt.t list)  (context: ctx_t): return =
  match context with (out, mon, verbose) ->(
    match stmts with
    | [] ->  raise(Except "Empty list")
    | s::stmts' -> ( let (return, label) = eval_small_step interceptor prog state stmts' verbose s in
                    if (mon = "nsu") then (
                     let mon_return : Mon.monitor_return = Mon.eval_small_step mon_state label in
                     (match mon_return with
                      | MReturn mon_state' -> (
                          match return with
                          |Finalv v ->  Finalv v
                          |Intermediate (state', stmts'') ->
                            print_string "CALLING MONITOR...\n";
                            small_step_iter interceptor prog state' mon_state' stmts'' context)
                      | MFail  (mon_state', str) ->
                        print_string ("MONITOR EXCEPTION -> "^str);
                        exit 1;))
                  else (
                    match return with
                          |Finalv v ->  Finalv v
                          |Intermediate (state', stmts'') ->
                            small_step_iter interceptor prog state' mon_state stmts'' context
                  )))
  


let initial_state () : state_t =
  let sto = Store.create [] in
  let cs = Callstack.push [] (Callstack.Toplevel) in
  let heap = Heap.create () in
  (cs, heap, sto)



(*Worker class of the Interpreter*)
let eval_prog (prog : Prog.t) (context : ctx_t) (main:string) : (Val.t option * Heap.t) =
  match context with (out, mon, verbose) -> 
    let func = (Prog.get_func prog main) in
    let state_0 = initial_state () in
    let mon_state_0 = Mon.initial_monitor_state () in
    let interceptor = SecLabel.interceptor Mon.parse_lvl in
    let v = small_step_iter interceptor prog state_0 mon_state_0 [func.body] context in
    (*let v=  small_step_iter prog cs heap sto func.body verbose in*)
    let _, heap, _ = state_0 in
    match v with
    | Finalv v -> v, heap
    | _ -> raise(Except "No return value")(*ERROR*)

end 
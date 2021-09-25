open Logging

module M
  (Mon : SecurityMonitor.M) = struct


exception Except of string

type state_t = Callstack.t * Heap.t * Store.t * string

type return =
  | Intermediate of state_t * Stmt.t list
  | Errorv of Val.t option
  | Finalv of Val.t option

type ctx_t = string * string * bool


let add_fields_to (obj : Object.t) (fes : (Field.t * Expr.t) list) (eval_e : (Expr.t -> Val.t)) : unit =
  List.iter (fun (f, e) -> let e' = eval_e e in Object.set obj f e') fes


let eval_unop (op : Oper.uopt) (v : Val.t) : Val.t =
  match op with
  | Neg           -> Oper.neg v
  | Not           -> Oper.not v
  | BitwiseNot    -> Oper.bitwise_not v
  | Typeof        -> Oper.typeof v
  | ListLen       -> Oper.l_len v
  | TupleLen      -> Oper.t_len v
  | StringLen     -> Oper.s_len v
  | StringLenU    -> Oper.s_len_u v
  | Head          -> Oper.head v
  | Tail          -> Oper.tail v
  | First         -> Oper.first v
  | Second        -> Oper.second v
  | LRemoveLast   -> Oper.list_remove_last v
  | LSort         -> Oper.list_sort v
  | LReverse -> Oper.list_reverse (v)
  | IntToFloat    -> Oper.int_to_float v
  | IntToString   -> Oper.int_to_string v
  | IntToFourHex  -> Oper.int_to_four_hex v
  | IntOfString   -> Oper.int_of_string v
  | IntOfFloat    -> Oper.int_of_float v
  | FloatToString -> Oper.float_to_string v
  | FloatOfString -> Oper.float_of_string v
  | HexDecode     -> Oper.hex_decode v
  | Utf8Decode    -> Oper.utf8_decode v
  | OctalToDecimal-> Oper.octal_to_decimal v
  | Sconcat       -> Oper.string_concat v
  | ObjToList     -> raise (Failure "Unexpected call to Core_Interpreter.eval_unop with operator ObjToList")
  | ObjFields     -> raise (Failure "Unexpected call to Core_Interpreter.eval_unop with operator ObjFields")
  | ToInt         -> Oper.to_int v
  | ToInt32       -> Oper.to_int32 v
  | ToUint32      -> Oper.to_uint32 v
  | FromCharCode  -> Oper.from_char_code v
  | FromCharCodeU -> Oper.from_char_code_u v
  | ToCharCode    -> Oper.to_char_code v
  | ToCharCodeU   -> Oper.to_char_code_u v
  | ToLowerCase   -> Oper.to_lower_case v
  | ToUpperCase   -> Oper.to_upper_case v
  | Trim          -> Oper.trim v
  | ToUint16      -> Oper.to_uint16 v
  | ParseNumber   -> Oper.parse_number v
  | ParseString   -> Oper.parse_string v
  | ParseDate   -> Oper.parse_date v
  | Log_2         -> Oper.log_2 v
  | Float64ToLEBytes -> Oper.float64_to_le_bytes v
  | Float64ToBEBytes -> Oper.float64_to_be_bytes v
  | Float32ToLEBytes -> Oper.float32_to_le_bytes v
  | Float32ToBEBytes -> Oper.float32_to_be_bytes v
  | Float64FromLEBytes -> Oper.float64_from_le_bytes v
  | Float64FromBEBytes -> Oper.float64_from_be_bytes v
  | Float32FromLEBytes -> Oper.float32_from_le_bytes v
  | Float32FromBEBytes -> Oper.float32_from_be_bytes v
  | BytesToString     -> Oper.bytes_to_string v
  | _             -> Oper.apply_uopt_oper op v

let eval_binopt_expr (op : Oper.bopt) (v1 : Val.t) (v2 : Val.t) : Val.t =
  match op with
  | Plus     -> Oper.plus (v1, v2)
  | Minus    -> Oper.minus (v1, v2)
  | Times    -> Oper.times (v1, v2)
  | Div      -> Oper.div (v1, v2)
  | Modulo   -> Oper.modulo (v1, v2)
  | Equal    -> Oper.equal (v1, v2)
  | Gt       -> Oper.gt (v1, v2)
  | Lt       -> Oper.lt (v1, v2)
  | Egt      -> Oper.egt (v1, v2)
  | Elt      -> Oper.elt (v1, v2)
  | Log_And  -> Oper.log_and (v1, v2)
  | Log_Or   -> Oper.log_or (v1, v2)
  | BitwiseAnd -> Oper.bitwise_and (v1, v2)
  | BitwiseOr -> Oper.bitwise_or (v1, v2)
  | BitwiseXor -> Oper.bitwise_xor (v1, v2)
  | ShiftLeft -> Oper.shift_left (v1, v2)
  | ShiftRight -> Oper.shift_right (v1, v2)
  | ShiftRightLogical -> Oper.shift_right_logical (v1, v2)
  | Lnth     -> Oper.list_nth (v1, v2)
  | Tnth     -> Oper.tuple_nth (v1, v2)
  | Snth     -> Oper.s_nth (v1,v2)
  | Snth_u   -> Oper.s_nth_u (v1,v2)
  | Ssplit   -> Oper.string_split (v1, v2)
  | Ladd     -> Oper.list_add (v1, v2)
  | Lprepend -> Oper.list_prepend (v1, v2)
  | Lconcat  -> Oper.list_concat (v1, v2)
  | InList   -> Oper.list_in (v1, v2)
  | InObj    -> raise(Except "Not expected")
  | ToPrecision -> Oper.to_precision (v1, v2)
  | ToExponential -> Oper.to_exponential (v1, v2)
  | ToFixed -> Oper.to_fixed (v1, v2)
  | _        -> Oper.apply_bopt_oper op v1 v2

let eval_triopt_expr (op : Oper.topt) (v1 : Val.t) (v2 : Val.t) (v3 : Val.t) : Val.t =
  match op with
  | Ssubstr  -> Oper.s_substr (v1,v2,v3)
  | SsubstrU  -> Oper.s_substr_u (v1,v2,v3)

let eval_nopt_expr (op : Oper.nopt) (vals : Val.t list) : Val.t =
  match op with
  | ListExpr  -> Val.List vals
  | TupleExpr -> Val.Tuple vals
  | NAry_And  -> Val.Bool (List.for_all Oper.is_true vals)
  | NAry_Or   -> Val.Bool (List.exists Oper.is_true vals)


let rec eval_expr (sto : Store.t) (e : Expr.t) : Val.t =
  match e with
  | Val n                -> n
  | Var x                ->
    (match Store.get sto x with
      | Some v -> v
      | None ->
          let msg = Printf.sprintf "Cannot find variable %s" x in
          raise (Failure msg))
  | UnOpt (uop, e)       -> let v = eval_expr sto e in
    eval_unop uop v
  | BinOpt (bop, e1, e2) ->
    let v1 = eval_expr sto e1 in
    let v2 = eval_expr sto e2 in
    eval_binopt_expr bop v1 v2
  | TriOpt (top, e1, e2, e3) ->
    let v1 = eval_expr sto e1 in
    let v2 = eval_expr sto e2 in
    let v3 = eval_expr sto e3 in
    eval_triopt_expr top v1 v2 v3
  | NOpt (nop, es)       -> eval_nopt_expr nop (List.map (eval_expr sto) es)
  | Curry (f, es)        ->
      let fv = eval_expr sto f in
      let vs = List.map (eval_expr sto) es in
      (match fv with
        | Str s -> Val.Curry (s, vs)
        | _     -> failwith "Illegal Curry Expression")

let get_func_id (sto:Store.t) (exp:Expr.t) : (string * Val.t list) =
  let res= eval_expr sto exp in
  match res with
  | Val.Str f -> (f, [])
  | Val.Curry (f, vs) -> (f, vs)
  | _ -> raise (Except "Wrong/Invalid Function ID")

let prepare_call (prog:Prog.t) (calling_f : string) (cs:Callstack.t) (sto: Store.t) (cont: Stmt.t list) (x:string) (es:Expr.t list) (f:string) (vs: Val.t list) : (Callstack.t * Store.t * string list) =
  let cs' = Callstack.push cs (Callstack.Intermediate (cont, sto, x, calling_f)) in
  let params = Prog.get_params prog f in
  let pvs = try
    List.combine params vs
  with _ -> raise (Failure ("Invalid number of arguments: " ^ f)) in
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


(* \/ ======================================= Main InterpreterFunctions ======================================= \/ *)


let eval_small_step (interceptor: string -> Val.t list -> Expr.t list -> (Mon.sl SecLabel.t) option) (prog: Prog.t) (state : state_t) (cont: Stmt.t list) (verbose: bool) (s: Stmt.t) : (return * (Mon.sl SecLabel.t))  =
  let (cs, heap, sto, f) = state in
  let str_e (e : Expr.t) : string = Val.str (eval_expr sto e) in
  if Stmt.is_basic_stmt s then
    print_endline (lazy (Printf.sprintf "====================================\nEvaluating >>>>> %s: %s (%s)" f (Stmt.str s) (Stmt.str ~print_expr:str_e s)));

  match s with
  | Skip ->
    (Intermediate ((cs, heap, sto, f), cont), SecLabel.EmptyLab)

  | Exception str -> (print_string "Exception thrown:\n";
                      print_string str;
                      exit 1)

  | Merge -> (Intermediate ((cs, heap, sto, f), cont), SecLabel.MergeLab)

  | Print e ->
    let v = eval_expr sto e in
    (match v with
    | Loc l ->
      (match Heap.get heap l with
        | Some o -> print_endline (lazy ("PROGRAM PRINT: " ^ Object.str o))
        | None   -> print_endline (lazy "PROGRAM PRINT: Non-existent location" ))
    | _     -> print_endline (lazy ("PROGRAM PRINT: " ^ (Val.str v))));
     (Intermediate ((cs, heap, sto, f), cont), SecLabel.PrintLab (e))

  | Fail e -> (
      let v = eval_expr sto e in
      Errorv (Some v), SecLabel.EmptyLab
    )

  | Assign (x,e) ->
    (let v = eval_expr sto e in
     Store.set sto x v;
     (Intermediate ((cs, heap, sto, f), cont), SecLabel.AssignLab (x,e)))


  | Return e ->
    let v = eval_expr sto e in
    let  (f,cs') = Callstack.pop cs in (
      match f with
      | Callstack.Intermediate (cont',sto', x, f') ->   (Store.set sto'  x v;
                                                     (Intermediate ((cs', heap, sto', f'), cont'), SecLabel.ReturnLab e))


      | Callstack.Toplevel -> (Finalv (Some v), SecLabel.ReturnLab e))

  | Block block ->
    (Intermediate ((cs, heap, sto, f), (block @ cont)), SecLabel.EmptyLab )



  | If (e,s1,s2) ->
    let v = eval_expr sto e in
    if (Oper.is_true v) then
      match s1 with
      | Block block ->
        let blockm = (block @ (Stmt.Merge :: [])) in
        (match s2 with
         |Some v -> Intermediate ((cs, heap, sto, f), (blockm @ cont)), SecLabel.BranchLab (e,v)
         |None   -> Intermediate ((cs, heap, sto, f), (blockm @ cont)), SecLabel.BranchLab (e,(Stmt.Skip)))
      | _ -> raise (Except "IF block expected ")

    else
      (match s2 with
       | Some v ->
         (match v with
          | Block block2 ->
          let block2m = (block2 @ (Stmt.Merge :: [])) in
          Intermediate ((cs, heap, sto, f), (block2m  @ cont)), SecLabel.BranchLab (e,s1)
          |_ -> raise (Except "Not expected"))
       | None ->
         (Intermediate ((cs, heap, sto, f), cont), SecLabel.EmptyLab))



  | While (e,s) ->
    let s1 = (s :: []) @ (Stmt.While (e,s) :: []) in
    let stms= Stmt.If (e, (Stmt.Block s1), None) in
    (Intermediate ((cs, heap, sto, f), (stms :: cont)), SecLabel.EmptyLab)

  | AssignCall (x,func,es) ->
    let (f', vs') = get_func_id sto func in
    let vs'' = (List.map (eval_expr sto) es) in
    let vs = vs' @ vs'' in
    let b = interceptor f' vs es in
    ( match b with
      |None ->
        let func = (Prog.get_func prog f') in
        let (cs', sto_aux, params) = prepare_call prog f cs sto cont x es f' vs in
        (let (cont':Stmt.t) = func.body in
         let aux_list= (cont'::[]) in
         (Intermediate ((cs', heap, sto_aux, f'), aux_list), SecLabel.AssignCallLab (params, es, x, f')))
      |Some lab ->
        (Intermediate((cs, heap, sto, f), cont),lab))

  | AssignECall (x,func,es) ->
    let vs = List.map (eval_expr sto) es in
    let v = External.execute prog heap func vs in
    Store.set sto x v;
    Intermediate ((cs, heap, sto, f), cont), SecLabel.EmptyLab

  | AssignInObjCheck (st, e_f, e_o) ->
    let field = eval_expr sto e_f in
    let obj = eval_expr sto e_o in
    let field', obj' =
      match field, obj with
      | Str f, Loc o -> f, o
      | _ -> raise (Except "Internal Error") in
    let v = eval_inobj_expr prog heap sto field obj in
    Store.set sto st v;
    (Intermediate ((cs, heap, sto, f), cont), SecLabel.AssignInObjCheckLab (st, field', obj', e_f, e_o))


  | AssignNewObj (x) ->
    let newobj= Object.create () in
    let loc= Heap.insert heap newobj in
    Store.set sto x (Val.Loc loc);
    (Intermediate ((cs, heap, sto, f), cont), SecLabel.NewLab (x, loc))


  | FieldLookup (x, e_o, e_f) ->
    let loc= eval_expr sto e_o in
    let field = eval_expr sto e_f in
    (match loc,field with
     | Loc loc', Str field' ->
       (let v = Heap.get_field heap loc' field' in
        let v' =(match v with
            | None     -> Val.Symbol "undefined"
            | Some v'' -> v''
          ) in
        Store.set sto x v';
        (Intermediate ((cs, heap, sto, f), cont), SecLabel.FieldLookupLab (x, loc', field', e_o, e_f)))
     | _                    ->
       invalid_arg ("Exception in Interpreter.eval_access_expr : \"e\" didn't evaluate to Loc."))


  | FieldAssign (e_o, e_f, e_v) ->
    let loc = eval_expr sto e_o in
    let field = eval_expr sto e_f in
    let v = eval_expr sto e_v in
    (match loc, field with
     | Loc loc , Str field ->
       Heap.set_field heap loc field v;
       (Intermediate ((cs, heap, sto, f), cont), SecLabel.FieldAssignLab (loc,field, e_o, e_f, e_v))
     | _       ->
       invalid_arg "Exception in Interpreter.eval_fieldassign_stmt : \"e_o\" is not a Loc value")



  | FieldDelete (e, e_f) ->
    let loc = eval_expr sto e in
    let field = eval_expr sto e_f in
    (match loc, field with
     | Loc loc', Str field' ->
       Heap.delete_field heap loc' field';
       (Intermediate ((cs, heap, sto, f), cont), SecLabel.FieldDeleteLab (loc', field', e, e_f))
     | _ -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc value")


  | AssignObjToList (st, e) ->
    let v = eval_objtolist_oper heap sto e in
    Store.set sto st v;
    (Intermediate ((cs, heap, sto, f), cont), SecLabel.AssignLab (st, e))


  | AssignObjFields (st, e) ->
    let v = eval_objfields_oper heap sto e in
    Store.set sto st v;
    (Intermediate ((cs, heap, sto, f), cont), SecLabel.AssignLab (st, e))


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
                          | Finalv v -> Finalv v
                          | Errorv v -> Errorv v
                          | Intermediate (state', stmts'') ->
                            small_step_iter interceptor prog state' mon_state' stmts'' context)
                      | MFail  (mon_state', str) ->
                        print_string ("MONITOR EXCEPTION -> "^str);
                        exit 1;))
                  else (
                    match return with
                          | Finalv v ->  Finalv v
                          (* | Finalv v -> (match v with
                              | Some v -> (match v with
                                  | Tuple t -> Finalv (Some (List.nth t 1))
                                  | _       -> Finalv (Some v))
                              | None   -> Finalv v) *)
                          | Errorv v -> Errorv v
                          | Intermediate (state', stmts'') ->
                            small_step_iter interceptor prog state' mon_state stmts'' context
                  )))



let initial_state () : state_t =
  let sto = Store.create [] in
  let cs = Callstack.push [] (Callstack.Toplevel) in
  let heap = Heap.create () in
  (cs, heap, sto, "main")



(*Worker class of the Interpreter*)
let eval_prog (prog : Prog.t) (context : ctx_t) (main : string) : (Val.t option * Heap.t) =
  match context with (out, mon, verbose) ->
    let func = (Prog.get_func prog main) in
    let state_0 = initial_state () in
    let mon_state_0 = Mon.initial_monitor_state () in
    let interceptor = SecLabel.interceptor Mon.parse_lvl in
    let v = small_step_iter interceptor prog state_0 mon_state_0 [func.body] context in
    let _, heap, _, _= state_0 in
    match v with
    | Finalv v -> v, heap
    | Errorv (Some (Val.Str s)) -> 
      let subStr = String.sub s 0 11 in
      print_endline  (lazy subStr);
      if (subStr = "Unsupported") then (Some (Val.Str s)), heap
      else (print_endline (lazy "eval_prog else"); raise(Except s))  
    | _ -> raise(Except "No return value")(*ERROR*)

end

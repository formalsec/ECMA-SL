open Val
open Expr
open Stmt
open Func
open Source
open Operators
open Eval_op

let ( let+ ) o f = match o with Ok v -> f v | Error m -> failwith m

module type SecurityMonitor = sig
  type state_t
  type sl

  type monitor_return =
    | MReturn of state_t
    | MFail of (state_t * string)

  val eval_small_step : state_t -> sl SecLabel.t -> monitor_return
  val initial_monitor_state : unit -> state_t
  val parse_lvl : string -> sl
end

module M (Mon : SecurityMonitor) = struct
  exception Except of string

  type stack_t = Val.t Store.t Call_stack.t
  type state_t = stack_t * Val.t Heap.t * Val.t Store.t * string

  type return =
    | Intermediate of state_t * Stmt.t list
    | Errorv of Val.t option
    | Finalv of Val.t option

  let add_fields_to (obj : Val.t Object.t) (fes : (Field.t * Expr.t) list)
    (eval_e : Expr.t -> Val.t) : unit =
    List.iter
      (fun (f, e) ->
        let e' = eval_e e in
        Object.set obj f e' )
      fes

  let rec eval_expr (sto : Val.t Store.t) (e : Expr.t) : Val.t =
    match e with
    | Val n -> n
    | Var x -> (
      match Store.get sto x with
      | Some v -> v
      | None ->
        let msg = Printf.sprintf "Cannot find variable %s" x in
        raise (Failure msg) )
    | UnOpt (uop, e) ->
      let v = eval_expr sto e in
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
    | NOpt (nop, es) -> eval_nopt_expr nop (List.map (eval_expr sto) es)
    | Curry (f, es) -> (
      let fv = eval_expr sto f in
      let vs = List.map (eval_expr sto) es in
      match fv with
      | Str s -> Val.Curry (s, vs)
      | _ -> failwith "Illegal Curry Expression" )
    | Symbolic (t, _) -> (
      match t with
      | Type.IntType ->
        Random.self_init ();
        Val.Int (Random.int 128)
      | Type.FltType ->
        Random.self_init ();
        Val.Flt (Random.float 128.0)
      | _ ->
        failwith
          (Core.sprintf "eval_expr: Symbolic \"%s\" not implemented!"
             (Type.str t) ) )

  let get_func_id (sto : Val.t Store.t) (exp : Expr.t) : string * Val.t list =
    let res = eval_expr sto exp in
    match res with
    | Val.Str f -> (f, [])
    | Val.Curry (f, vs) -> (f, vs)
    | _ -> raise (Except "Wrong/Invalid Function ID")

  let prepare_call (prog : Prog.t) (calling_f : string) (cs : stack_t)
    (sto : Val.t Store.t) (cont : Stmt.t list) (x : string) (_es : Expr.t list)
    (f : string) (vs : Val.t list) : stack_t * Val.t Store.t * string list =
    let cs' =
      Call_stack.push cs (Call_stack.Intermediate (cont, sto, x, calling_f))
    in
    let+ params = Prog.get_params prog f in
    let pvs =
      try List.combine params vs
      with _ -> raise (Failure ("Invalid number of arguments: " ^ f))
    in
    let sto_aux = Store.create pvs in
    (cs', sto_aux, params)

  let eval_inobj_expr (_prog : Prog.t) (heap : Val.t Heap.t)
    (_sto : Val.t Store.t) (field : Val.t) (loc : Val.t) : Val.t =
    let b =
      match (loc, field) with
      | Loc l, Str f -> Heap.get_field heap l f
      | _ ->
        invalid_arg
          "Exception in Interpreter.eval_inobj_expr : \"loc\" is not a Loc \
           value or \"field\" is not a string"
    in
    match b with Some _v -> Bool true | None -> Bool false

  let eval_objtolist_oper (heap : Val.t Heap.t) (st : Val.t Store.t)
    (loc_expr : Expr.t) : Val.t =
    let loc = eval_expr st loc_expr in
    let loc' =
      match loc with
      | Loc l -> l
      | _ ->
        invalid_arg
          "Exception in Interpreter.eval_objtolist_oper: \"loc\" is not a Loc \
           value"
    in
    let obj = Heap.get heap loc' in
    match obj with
    | None ->
      invalid_arg
        ( "Exception in Interpreter.eval_objtolist_oper: \""
        ^ Loc.str loc'
        ^ "\" doesn't exist in the Heap" )
    | Some o ->
      let fvs = Object.to_list o in
      List (List.map (fun (f, v) -> Val.Tuple (Str f :: [ v ])) fvs)

  let eval_objfields_oper (heap : Val.t Heap.t) (st : Val.t Store.t)
    (loc_expr : Expr.t) : Val.t =
    let loc = eval_expr st loc_expr in
    let loc' =
      match loc with
      | Loc l -> l
      | _ ->
        invalid_arg
          "Exception in Interpreter.eval_objfields_oper: \"loc\" is not a Loc \
           value"
    in
    let obj = Heap.get heap loc' in
    match obj with
    | None ->
      invalid_arg
        ( "Exception in Interpreter.eval_objfields_oper: \""
        ^ Loc.str loc'
        ^ "\" doesn't exist in the Heap" )
    | Some o -> List (List.map (fun f -> Val.Str f) (Object.get_fields o))

  let eval_fielddelete_stmt (_prog : Prog.t) (heap : Val.t Heap.t)
    (sto : Val.t Store.t) (e : Expr.t) (f : Expr.t) : unit =
    let loc = eval_expr sto e
    and field = eval_expr sto f in
    let loc' =
      match loc with
      | Loc loc -> loc
      | _ ->
        invalid_arg
          "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc \
           value"
    in
    let field' =
      match field with
      | Str field -> field
      | _ ->
        invalid_arg
          "Exception in Interpreter.eval_fielddelete_stmt : \"f\" didn't \
           evaluate to Str"
    in
    Heap.delete_field heap loc' field'

  (* \/ ======================================= Main InterpreterFunctions ======================================= \/ *)

  let eval_small_step
    (interceptor :
      string -> Val.t list -> Expr.t list -> Mon.sl SecLabel.t option )
    (prog : Prog.t) (state : state_t) (cont : Stmt.t list) (s : Stmt.t) :
    return * Mon.sl SecLabel.t =
    let cs, heap, sto, f = state in
    let str_e (e : Expr.t) : string = Val.str (eval_expr sto e) in
    if Stmt.is_basic_stmt s then
      Log.debug
        "====================================\nEvaluating >>>>> %s: %s (%s)" f
        (Stmt.str s)
        (Stmt.str ~print_expr:str_e s);
    match s.it with
    | Skip -> (Intermediate ((cs, heap, sto, f), cont), SecLabel.EmptyLab)
    | Exception str ->
      print_string (Source.string_of_region s.at ^ ": Exception: " ^ str ^ "\n");
      exit 1
    | Merge -> (Intermediate ((cs, heap, sto, f), cont), SecLabel.MergeLab)
    | Print e ->
      let v = eval_expr sto e in
      ( match v with
      | Loc l -> (
        match Heap.get heap l with
        | Some o ->
          Log.debug "PROGRAM PRINT: %s"
            (Object.to_string o (Val.str ~flt_with_dot:false))
        | None -> Log.debug "PROGRAM PRINT: Non-existent location" )
      | _ -> Log.debug "PROGRAM PRINT: %s" (Val.str v) );
      (Intermediate ((cs, heap, sto, f), cont), SecLabel.PrintLab e)
    | Abort _ ->
      (* NOP *)
      (Intermediate (state, cont), SecLabel.EmptyLab)
    | Fail e ->
      let str_e (e : Expr.t) : string = Val.str (eval_expr sto e) in
      Log.debug
        "====================================\n\
         Evaluating >>>>> %s: %s (%s) in the callstack:\n\
        \ %s" f (Stmt.str s)
        (Stmt.str ~print_expr:str_e s)
        (Call_stack.str cs);
      let v = eval_expr sto e in
      (Errorv (Some v), SecLabel.EmptyLab)
    | Assert e ->
      let v = eval_expr sto e in
      if is_true v then (Intermediate (state, cont), SecLabel.EmptyLab)
      else
        let e' = "Assert false: " ^ Expr.str e in
        (Errorv (Some (Val.Str e')), SecLabel.EmptyLab)
    | Assign (x, e) ->
      let v = eval_expr sto e in
      Store.set sto x v;
      (Intermediate ((cs, heap, sto, f), cont), SecLabel.AssignLab (x, e))
    | Return e -> (
      let v = eval_expr sto e in
      let f, cs' = Call_stack.pop cs in
      match f with
      | Call_stack.Intermediate (cont', sto', x, f') ->
        Store.set sto' x v;
        (Intermediate ((cs', heap, sto', f'), cont'), SecLabel.ReturnLab e)
      | Call_stack.Toplevel -> (Finalv (Some v), SecLabel.ReturnLab e) )
    | Block block ->
      (Intermediate ((cs, heap, sto, f), block @ cont), SecLabel.EmptyLab)
    | If (e, s1, s2) -> (
      let v = eval_expr sto e in
      if is_true v then
        match s1.it with
        | Block block -> (
          let blockm = block @ ((Stmt.Merge @> s1.at) :: []) in
          match s2 with
          | Some v ->
            ( Intermediate ((cs, heap, sto, f), blockm @ cont)
            , SecLabel.BranchLab (e, v) )
          | None ->
            ( Intermediate ((cs, heap, sto, f), blockm @ cont)
            , SecLabel.BranchLab (e, Stmt.Skip @> s.at) ) )
        | _ -> raise (Except "IF block expected ")
      else
        match s2 with
        | Some v -> (
          match v.it with
          | Block block2 ->
            let block2m = block2 @ ((Stmt.Merge @> v.at) :: []) in
            ( Intermediate ((cs, heap, sto, f), block2m @ cont)
            , SecLabel.BranchLab (e, s1) )
          | _ -> raise (Except "Not expected") )
        | None -> (Intermediate ((cs, heap, sto, f), cont), SecLabel.EmptyLab) )
    | While (e, s') ->
      let s1 = (s' :: []) @ ((Stmt.While (e, s') @> s.at) :: []) in
      let stms = Stmt.If (e, Stmt.Block s1 @> s'.at, None) @> s.at in
      (Intermediate ((cs, heap, sto, f), stms :: cont), SecLabel.EmptyLab)
    | AssignCall (x, func, es) -> (
      let f', vs' = get_func_id sto func in
      let vs'' = List.map (eval_expr sto) es in
      let vs = vs' @ vs'' in
      let b = interceptor f' vs es in
      match b with
      | None ->
        let+ func = Prog.get_func prog f' in
        let cs', sto_aux, params = prepare_call prog f cs sto cont x es f' vs in
        let (cont' : Stmt.t) = func.body in
        let aux_list = cont' :: [] in
        ( Intermediate ((cs', heap, sto_aux, f'), aux_list)
        , SecLabel.AssignCallLab (params, es, x, f') )
      | Some lab -> (Intermediate ((cs, heap, sto, f), cont), lab) )
    | AssignECall (x, func, es) ->
      let vs = List.map (eval_expr sto) es in
      let v = External.execute prog heap func vs in
      Store.set sto x v;
      (Intermediate ((cs, heap, sto, f), cont), SecLabel.EmptyLab)
    | AssignInObjCheck (st, e_f, e_o) ->
      let field = eval_expr sto e_f in
      let obj = eval_expr sto e_o in
      let field', obj' =
        match (field, obj) with
        | Str f, Loc o -> (f, o)
        | _ -> raise (Except "Internal Error")
      in
      let v = eval_inobj_expr prog heap sto field obj in
      Store.set sto st v;
      ( Intermediate ((cs, heap, sto, f), cont)
      , SecLabel.AssignInObjCheckLab (st, field', obj', e_f, e_o) )
    | AssignNewObj x ->
      let newobj = Object.create () in
      let loc = Heap.insert heap newobj in
      Store.set sto x (Val.Loc loc);
      (Intermediate ((cs, heap, sto, f), cont), SecLabel.NewLab (x, loc))
    | FieldLookup (x, e_o, e_f) -> (
      let loc = eval_expr sto e_o in
      let field = eval_expr sto e_f in
      match (loc, field) with
      | Loc loc', Str field' ->
        let v = Heap.get_field heap loc' field' in
        let v' =
          match v with None -> Val.Symbol "undefined" | Some v'' -> v''
        in
        Store.set sto x v';
        ( Intermediate ((cs, heap, sto, f), cont)
        , SecLabel.FieldLookupLab (x, loc', field', e_o, e_f) )
      | _ ->
        invalid_arg
          "Exception in Interpreter.eval_access_expr : \"e\" didn't evaluate \
           to Loc." )
    | FieldAssign (e_o, e_f, e_v) -> (
      let loc = eval_expr sto e_o in
      let field = eval_expr sto e_f in
      let v = eval_expr sto e_v in
      match (loc, field) with
      | Loc loc, Str field ->
        Heap.set_field heap loc field v;
        ( Intermediate ((cs, heap, sto, f), cont)
        , SecLabel.FieldAssignLab (loc, field, e_o, e_f, e_v) )
      | _ ->
        invalid_arg
          "Exception in Interpreter.eval_fieldassign_stmt : \"e_o\" is not a \
           Loc value" )
    | FieldDelete (e, e_f) -> (
      let loc = eval_expr sto e in
      let field = eval_expr sto e_f in
      match (loc, field) with
      | Loc loc', Str field' ->
        Heap.delete_field heap loc' field';
        ( Intermediate ((cs, heap, sto, f), cont)
        , SecLabel.FieldDeleteLab (loc', field', e, e_f) )
      | _ ->
        invalid_arg
          "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc \
           value" )
    | AssignObjToList (st, e) ->
      let v = eval_objtolist_oper heap sto e in
      Store.set sto st v;
      (Intermediate ((cs, heap, sto, f), cont), SecLabel.AssignLab (st, e))
    | AssignObjFields (st, e) ->
      let v = eval_objfields_oper heap sto e in
      Store.set sto st v;
      (Intermediate ((cs, heap, sto, f), cont), SecLabel.AssignLab (st, e))

  (*This function will iterate smallsteps in a list of functions*)
  let rec small_step_iter
    (interceptor :
      string -> Val.t list -> Expr.t list -> Mon.sl SecLabel.t option )
    (prog : Prog.t) (state : state_t) (mon_state : Mon.state_t)
    (stmts : Stmt.t list) (monitor : string) : return =
    match stmts with
    | [] -> raise (Except "Empty list")
    | s :: stmts' -> (
      let return, label = eval_small_step interceptor prog state stmts' s in
      if monitor = "nsu" then (
        let mon_return : Mon.monitor_return =
          Mon.eval_small_step mon_state label
        in
        match mon_return with
        | Mon.MReturn mon_state' -> (
          match return with
          | Finalv v -> Finalv v
          | Errorv v -> Errorv v
          | Intermediate (state', stmts'') ->
            small_step_iter interceptor prog state' mon_state' stmts'' monitor )
        | Mon.MFail (_mon_state', str) ->
          print_string ("MONITOR EXCEPTION -> " ^ str);
          exit 1 )
      else
        match return with
        | Finalv v -> Finalv v
        (* | Finalv v -> (match v with
            | Some v -> (match v with
                | Tuple t -> Finalv (Some (List.nth t 1))
                | _       -> Finalv (Some v))
            | None   -> Finalv v) *)
        | Errorv v -> Errorv v
        | Intermediate (state', stmts'') ->
          small_step_iter interceptor prog state' mon_state stmts'' monitor )

  let initial_state () : state_t =
    let sto = Store.create [] in
    let cs = Call_stack.push Call_stack.empty Call_stack.Toplevel in
    let heap = Heap.create () in
    (cs, heap, sto, "main")

  (*Worker class of the Interpreter*)
  let eval_prog ?(monitor : string = "") ?(main : string = "main")
    (prog : Prog.t) : Val.t option * Val.t Heap.t =
    let+ func = Prog.get_func prog main in
    let state_0 = initial_state () in
    let mon_state_0 = Mon.initial_monitor_state () in
    let interceptor = SecLabel.interceptor Mon.parse_lvl in
    let v =
      small_step_iter interceptor prog state_0 mon_state_0 [ func.body ] monitor
    in
    let _, heap, _, _ = state_0 in
    match v with
    | Finalv v -> (v, heap)
    | Errorv (Some (Val.Str s)) ->
      let subStr = String.sub s 0 11 in
      Log.debug "%s" subStr;
      if subStr = "Unsupported" then (Some (Val.Str s), heap)
      else (
        Log.debug "eval_prog else";
        raise (Except s) )
    | _ -> raise (Except "No return value")
  (*ERROR*)
end

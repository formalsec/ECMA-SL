(*
val eval_expr : SStore.t -> Expr.t -> SVal.t

type state_t = SCallStack.t * SHeap.t * SStore.t * string

type intermediate_t = ((state_t * Stmt.t) list * (state_t * SVal.t option) list * (state_t * SVal.t option) list) 

type return_t = ((state_t * SVal.t option) list * (state_t * SVal.t option) list)

let eval_small_step
    (prog : Prog.t) 
    (state : state_t) 
    (cont : Stmt.t list) 
    (s : Stmt.t) : (intermediate_t list, return_t list) =

  let (cs, hp, sto, f) = state in 

  match s with
    | Assign (x, e) ->
      let v = eval_expr sto e in
      SStore.set sto x v;
      ([ (state, cont) ], [], [])

let rec small_step_iter
  (prog : Prog.t) 
  (states : intermediate_t list)
  (finals : return_t list) : return_t list = 
*)
open Func

type config = {
  prog : Prog.t;
  code : outcome;
  state : state;
  pc : pc;
  solver : Z3.Solver.solver;
}

and outcome =
  | Cont of Stmt.t list
  | Error of Sval.t option
  | Final of Sval.t option

(* TODO:
   | AsrtFail of Sval.t option
   | Unknwon of Sval.t option
*)
and func = string
and stack = Sstore.t Call_stack.t
and state = Sheap.t * Sstore.t * stack * func
and pc = Sval.t list

exception Runtime_error of string

let is_error (o : outcome) : bool = match o with Error _ -> true | _ -> false
let is_final (o : outcome) : bool = match o with Final _ -> true | _ -> false

let rec eval_expression (store : Sstore.t) (e : Expr.t) : Sval.t =
  match e with
  | Expr.Val n -> Sval.of_val n
  | Expr.Var x -> (
      match Sstore.find_opt store x with
      | Some v -> v
      | None -> raise (Runtime_error ("Cannot find var '" ^ x ^ "'")))
  | Expr.UnOpt (op, e) ->
      let v = eval_expression store e in
      Eval_operators.eval_unop op v
  | Expr.BinOpt (op, e1, e2) ->
      let v1 = eval_expression store e1 and v2 = eval_expression store e2 in
      Eval_operators.eval_binop op v1 v2
  | Expr.TriOpt (op, e1, e2, e3) ->
      let v1 = eval_expression store e1
      and v2 = eval_expression store e2
      and v3 = eval_expression store e3 in
      Eval_operators.eval_triop op v1 v2 v3
  | Expr.NOpt (op, es) ->
      let vs = List.map (eval_expression store) es in
      Eval_operators.eval_nop op vs
  | Expr.Curry (_, _) ->
      raise (Runtime_error "eval_expression: 'Curry' not implemented!")
  | Expr.Symbolic (t, x) -> Sval.Symbolic (t, x)

let update (c : config) (o : outcome) (s : state) (pc : pc)
    (solver : Z3.Solver.solver) : config =
  { c with code = o; state = s; pc; solver }

let step (c : config) : config list =
  let { prog; code; state; pc; solver } = c in
  let heap, store, stack, f = state in
  let stmts =
    match code with
    | Cont stmts -> stmts
    | _ -> raise (Runtime_error "step: Empty continuation!")
  in
  let s = List.hd stmts in
  match s with
  | Stmt.Skip -> [ update c (Cont (List.tl stmts)) state pc solver ]
  | Stmt.Merge -> [ update c (Cont (List.tl stmts)) state pc solver ]
  | Stmt.Exception err ->
      [ update c (Error (Some (Sval.Str err))) state pc solver ]
  | Stmt.Print e ->
      print_endline "print ignored.";
      [ update c (Cont (List.tl stmts)) state pc solver ]
  | Stmt.Fail e ->
      [ update c (Error (Some (eval_expression store e))) state pc solver ]
  | Stmt.Abort e ->
      [ update c (Final (Some (eval_expression store e))) state pc solver ]
  | Stmt.Assume e ->
      let v = eval_expression store e in
      Encoding.add solver [ v ];
      if not (Encoding.check solver []) then
        [ update c (Final (Some v)) state pc solver ]
      else [ update c (Cont (List.tl stmts)) state (v :: pc) solver ]
  | Stmt.Assert e ->
      let v = eval_expression store e in
      let v' = eval_expression store (Expr.UnOpt (Operators.Not, e)) in
      if Encoding.check solver [ v' ] then
        [ update c (Error (Some v)) state pc solver ]
      else [ update c (Cont (List.tl stmts)) state pc solver ]
  | Stmt.Assign (x, e) ->
      let v = eval_expression store e in
      [
        update c
          (Cont (List.tl stmts))
          (heap, Sstore.add store x v, stack, f)
          pc solver;
      ]
  | Stmt.Block block ->
      [ update c (Cont (block @ List.tl stmts)) state pc solver ]
  | Stmt.If (cond, stmts1, stmts2) ->
      let cond' = eval_expression store cond
      and not_cond' = eval_expression store (Expr.UnOpt (Operators.Not, cond))
      and solver' = Encoding.clone solver in
      let then_branch =
        Encoding.add solver [ cond' ];
        if Encoding.check solver [] then
          match stmts1 with
          | Stmt.Block b ->
              let b' = b @ [ Stmt.Merge ] @ List.tl stmts in
              [ update c (Cont b') state (cond' :: pc) solver ]
          | _ -> raise (Runtime_error "Malformed if statement!")
        else []
      in
      let else_branch =
        Encoding.add solver' [ not_cond' ];
        if Encoding.check solver' [] then
          match stmts2 with
          | None ->
              [
                update c (Cont (List.tl stmts)) state (not_cond' :: pc) solver';
              ]
          | Some (Stmt.Block b) ->
              let b' = b @ [ Stmt.Merge ] @ List.tl stmts in
              [ update c (Cont b') state (not_cond' :: pc) solver' ]
          | Some _ -> raise (Runtime_error "Malformed if statement!")
        else []
      in
      then_branch @ else_branch
  | Stmt.While (cond, stmts') ->
      let stmts1 = Stmt.Block (stmts' :: [ Stmt.While (cond, stmts') ]) in
      [
        update c
          (Cont (Stmt.If (cond, stmts1, None) :: List.tl stmts))
          state pc solver;
      ]
  | Stmt.Return e -> (
      let v = eval_expression store e in
      let frame, stack' = Call_stack.pop stack in
      match frame with
      | Call_stack.Intermediate (stmts', store', x, f') ->
          [
            update c (Cont stmts')
              (heap, Sstore.add store' x v, stack', f')
              pc solver;
          ]
      | Call_stack.Toplevel -> [ update c (Final (Some v)) state pc solver ])
  | Stmt.AssignCall (f, e, es) ->
      raise (Runtime_error "Eval: step: 'AssignCall' not implemented!")
  | Stmt.AssignECall (x, y, es) ->
      raise (Runtime_error "Eval: step: 'AssignECall' not implemented!")
  | Stmt.AssignNewObj x ->
      let obj = Object.create () in
      let loc = Loc.newloc () in
      let loc' = Sval.Loc loc in
      [
        update c
          (Cont (List.tl stmts))
          (Sheap.add heap loc obj, Sstore.add store x loc', stack, f)
          pc solver;
      ]
  | Stmt.AssignInObjCheck (x, e1, e2) ->
      raise (Runtime_error "Eval: step: 'AssignInObjCheck' not implemented!")
  | Stmt.AssignObjToList (x, e) ->
      raise (Runtime_error "Eval: step: 'AssignObjToList' not implemented!")
  | Stmt.AssignObjFields (x, e) ->
      raise (Runtime_error "Eval: step: 'AssignObjFields' not implemented!")
  | Stmt.FieldAssign (e1, e2, e3) ->
      raise (Runtime_error "Eval: step: 'FieldAssign' not implemented!")
  | Stmt.FieldDelete (e1, e2) ->
      raise (Runtime_error "Eval: step: 'FieldDelete' not implemented!")
  | Stmt.FieldLookup (x, e1, e2) ->
      raise (Runtime_error "Eval: step: 'FieldLookup' not implemented!")

let rec eval (input_confs : config list) (output_confs : config list) :
    config list =
  match input_confs with
  | [] -> output_confs
  | c :: input_confs' -> (
      match c.code with
      | Cont [] -> raise (Runtime_error "eval: Empty continuation!")
      | Cont _ -> eval (step c @ input_confs') output_confs
      | Error v -> eval input_confs' (c :: output_confs)
      | Final v -> eval input_confs' (c :: output_confs))

let invoke (prog : Prog.t) (f : func) : config list =
  let func = Prog.get_func prog f in
  let heap = Sheap.create ()
  and store = Sstore.create []
  and stack = Call_stack.push Call_stack.empty Call_stack.Toplevel in
  let initial_config =
    {
      prog;
      code = Cont [ func.body ];
      state = (heap, store, stack, f);
      pc = [];
      solver = Encoding.mk_solver ();
    }
  in
  eval [ initial_config ] []

let analyse (prog : Prog.t) (f : func) : Report.t =
  let time_analysis = ref 0.0 in
  let out_configs =
    Time_utils.time_call time_analysis (fun () -> invoke prog f)
  in
  let final_configs = List.filter (fun c -> is_final c.code) out_configs
  and error_configs = List.filter (fun c -> is_error c.code) out_configs in
  let final_testsuite, error_testsuite =
    ( List.map (fun c -> Encoding.model c.solver) final_configs,
      List.map (fun c -> Encoding.model c.solver) error_configs )
  in
  let report =
    Report.create !Flags.file (List.length out_configs)
      (List.length error_configs)
      0 !time_analysis !Encoding.time_solver
  in
  Report.add_testsuites report final_testsuite error_testsuite

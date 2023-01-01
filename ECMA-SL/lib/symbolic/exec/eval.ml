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

type func = string
type stack = Sstore.t Call_stack.t
type state = Sheap.t * Sstore.t * stack * func

type outcome =
  | Cont of Stmt.t list
  | Error of Sval.t option
  | Final of Sval.t option
  (* TODO:
  | AsrtFail of Sval.t option
  | Unknwon of Sval.t option
  *)

type pc = Sval.t list
type config = { prog : Prog.t; code : outcome; state : state; pc : pc }

exception Runtime_error of string

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

let update (c : config) (o : outcome) (s : state) (pc : pc) : config =
  { c with code = o; state = s; pc }

let step (c : config) : config list =
  let { prog; code; state; pc } = c in
  let heap, store, stack, f = state in
  let stmts =
    match code with
    | Cont stmts -> stmts
    | _ -> raise (Runtime_error "step: Empty continuation!")
  in
  let s = List.hd stmts in
  match s with
  | Stmt.Skip -> [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Merge -> [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Exception err -> [ update c (Error (Some (Sval.Str err))) state pc ]
  | Stmt.Print e ->
      print_endline "print ignored.";
      [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Fail e ->
      [ update c (Error (Some (eval_expression store e))) state pc ]
  | Stmt.Abort e ->
      [ update c (Final (Some (eval_expression store e))) state pc ]
  | Stmt.Assume e ->
      let v = eval_expression store e in
      if not (Encoding.check (v :: pc)) then
        [ update c (Final (Some v)) state pc ]
      else [ update c (Cont (List.tl stmts)) state (v :: pc) ]
  | Stmt.Assert e ->
      let v = eval_expression store e in
      let v' = eval_expression store (Expr.UnOpt (Operators.Not, e)) in
      if Encoding.check (v' :: pc) then [ update c (Error (Some v)) state pc ]
      else [ update c (Cont (List.tl stmts)) state (v :: pc) ]
  | Stmt.Assign (x, e) ->
      let v = eval_expression store e in
      [
        update c
          (Cont (List.tl stmts))
          (heap, Sstore.add store x v, stack, f)
          pc;
      ]
  | Stmt.Block block -> [ update c (Cont (block @ List.tl stmts)) state pc ]
  | Stmt.If (cond, stmts1, stmts2) ->
      let cond' = eval_expression store cond
      and not_cond' = eval_expression store (Expr.UnOpt (Operators.Not, cond)) in
      let then_branch =
        if Encoding.check (cond' :: pc) then
          match stmts1 with
          | Stmt.Block b -> 
              let b' = b @ ([ Stmt.Merge ] @ List.tl stmts) in
              [ update c (Cont b') state (cond' :: pc) ]
          | _ -> raise (Runtime_error "Malformed if statement!")
        else []
      in
      let else_branch =
        if Encoding.check (not_cond' :: pc) then
          match stmts2 with
          | None -> [ update c (Cont (List.tl stmts)) state (not_cond' :: pc) ]
          | Some (Stmt.Block b) ->
              let b' = b @ ([ Stmt.Merge ] @ List.tl stmts) in
              [ update c (Cont b') state (not_cond' :: pc) ]
          | Some _ -> raise (Runtime_error "Malformed if statement!")
        else []
      in
      then_branch @ else_branch
  | Stmt.While (cond, stmts') ->
      let stmts1 = Stmt.Block (stmts' :: [ Stmt.While (cond, stmts') ]) in
      [
        update c (Cont (Stmt.If (cond, stmts1, None) :: List.tl stmts)) state pc;
      ]
  | Stmt.Return e -> (
      let v = eval_expression store e in
      let frame, stack' = Call_stack.pop stack in
      match frame with
      | Call_stack.Intermediate (stmts', store', x, f') ->
          [
            update c (Cont stmts') (heap, Sstore.add store' x v, stack', f') pc;
          ]
      | Call_stack.Toplevel -> [ update c (Final (Some v)) state pc ])
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
          pc;
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

let rec eval (input_confs : config list) (output_confs : config list) : config list =
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
  let state = (heap, store, stack, f) in
  eval [ { prog; code = Cont [ func.body ]; state; pc = [] } ] []

let analyse (prog : Prog.t) (f : func) : string list =
  let time_analysis = ref 0.0 in
  let outcomes = Time_utils.time_call time_analysis (fun () -> invoke prog f) in
  List.iter
    (fun c ->
      match c.code with Error v -> print_endline "Found Error!" | _ -> ())
    outcomes;
  [
    "\"file\" : \"" ^ !Flags.file  ^ "\"";
    "\"paths_total\" : " ^ (string_of_int (List.length outcomes));
    "\"paths_error\" : 0";
    "\"paths_unknown\" : 0";
    "\"time_analysis\" : \"" ^ string_of_float !time_analysis ^ "\"";
    "\"time_solver\" : \"" ^ string_of_float !Encoding.time_solver ^ "\"";
  ]


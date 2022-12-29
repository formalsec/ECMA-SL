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

type config = { prog : Prog.t; code : outcome; state : state; pc : Sval.t list }

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

let step (c : config) : config =
  let { prog; code; state; pc } = c in
  let heap, store, stack, f = state in
  let stmts =
    match code with
    | Cont stmts -> stmts
    | _ -> raise (Runtime_error "step: Empty continuation!")
  in
  let s = List.hd stmts in
  let code', state', pc' =
    match s with
    | Stmt.Skip -> (Cont (List.tl stmts), state, pc)
    | Stmt.Merge -> (Cont (List.tl stmts), state, pc)
    | Stmt.Exception err -> (Error (Some (Sval.Str err)), state, pc)
    | Stmt.Print e ->
        print_endline "print ignored.";
        (Cont (List.tl stmts), state, pc)
    | Stmt.Abort e -> (Final (Some (eval_expression store e)), state, pc)
    | Stmt.Fail e -> (Error (Some (eval_expression store e)), state, pc)
    | Stmt.Assume e ->
        let v = eval_expression store e in
        if not (Encoding.check (v :: pc)) then (Final (Some v), state, pc)
        else (Cont (List.tl stmts), state, v :: pc)
    | Stmt.Assert e ->
        let v = eval_expression store e in
        let v' = eval_expression store (Expr.UnOpt (Operators.Not, e)) in
        if Encoding.check (v' :: pc) then (Error (Some v), state, pc)
        else (Cont (List.tl stmts), state, v :: pc)
    | Stmt.Assign (x, e) ->
        let v = eval_expression store e in
        (Cont (List.tl stmts), (heap, Sstore.add store x v, stack, f), pc)
    | Stmt.Return e -> (
        let v = eval_expression store e in
        let frame, stack' = Call_stack.pop stack in
        match frame with
        | Call_stack.Intermediate (stmts', store', x, f') ->
            (Cont stmts', (heap, Sstore.add store' x v, stack', f'), pc)
        | Call_stack.Toplevel -> (Final (Some v), state, pc))
    | Stmt.Block block -> (Cont (block @ List.tl stmts), state, pc)
    | Stmt.AssignNewObj x ->
        let obj = Object.create () in
        let loc = Loc.newloc () in
        let loc' = Sval.Loc loc in
        ( Cont (List.tl stmts),
          (Sheap.add heap loc obj, Sstore.add store x loc', stack, f),
          pc )
    | _ -> failwith ("Seval: step: \"" ^ Stmt.str s ^ "\" not implemented!")
  in
  { c with code = code'; state = state'; pc = pc' }

let rec eval (c : config) : config =
  match c.code with
  | Cont [] -> raise (Runtime_error "eval: Empty continuation!")
  | Cont stmts -> eval (step c)
  | Error v -> (
      match v with
      | Some v' -> raise (Runtime_error ("eval: Runtime error: " ^ Sval.str v'))
      | None -> raise (Runtime_error "eval: Runtime error detected!"))
  | Final v -> c

let invoke (prog : Prog.t) (f : func) : config =
  let func = Prog.get_func prog f in
  let heap = Sheap.create ()
  and store = Sstore.create []
  and stack = Call_stack.push Call_stack.empty Call_stack.Toplevel in
  let state = (heap, store, stack, f) in
  eval { prog; code = Cont [ func.body ]; state; pc = [] }

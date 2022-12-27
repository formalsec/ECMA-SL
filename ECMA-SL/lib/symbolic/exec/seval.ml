(*
val eval_expr : SStore.t -> Expr.t -> SVal.t

type state_t = SCallStack.t * SHeap.t * SStore.t * string

type intermediate_t = ((state_t * Stmt.t) list * (state_t * SVal.t option) list * (state_t * SVal.t option) list) 

type return_t = ((state_t * SVal.t option) list * (state_t * SVal.t option) list)

let eval_small_step
    (prog : Prog.t) 
    (state : state_t) 
    (cont : Stmt.t list) 
    (s : Stmt.t) : intermediate_t list =
  
  let (cs, hp, sto, f) = state in 

  match s with      
    | Assign (x, e) ->
      let v = eval_expr sto e in
      SStore.set sto x v;
      ([ (state, cont) ], [], [])

let rec small_step_iter
  (prog : Prog.t) 
  (states : intermediate_t list) : return_t list = 
*)
open Func

type func = string
type stack = Sstore.t Call_stack.t 
type state = Heap.t * Sstore.t * stack * func

type outcome =
  | Cont of Stmt.t list
  | Error of Sval.t option
  | Final of Sval.t option

type config = {
  prog : Prog.t;
  code : outcome;
  state : state;
}

exception Runtime_error of string

let eval_expression (store : Sstore.t) (e : Expr.t) : Sval.t =
  Sval.Int 0

let step (c : config) : config =
  let { prog = prog; code = code; state = state } = c in
  let heap, store, stack, f = state in
  let stmts = match code with Cont stmts -> stmts | _ -> failwith "Empty Cont" in
  let s = List.hd stmts in
  let code', state' = match s with
    | Stmt.Skip -> Cont (List.tl stmts), state
    | Stmt.Merge -> Cont (List.tl stmts), state
    | Stmt.Exception err -> Error (Some (Sval.Str err)), state
    | Stmt.Print e -> 
        print_endline "print ignored.";
        Cont (List.tl stmts), state
    | Stmt.Abort e -> Final (Some (eval_expression store e)), state
    | Stmt.Fail e -> Error (Some (eval_expression store e)), state
    | Stmt.Assume e ->
        let _ = eval_expression store e in
        Cont (List.tl stmts), state
    | Stmt.Assert e ->
        let _ = eval_expression store e in
        Cont (List.tl stmts), state
    | Stmt.Return e -> (
        let v = eval_expression store e in
        let frame, stack' = Call_stack.pop stack in
        match frame with
        | Call_stack.Intermediate (stmts', store', x, f') ->
            Cont stmts', (heap, Sstore.add store' x v, stack', f')
        | Call_stack.Toplevel -> Final (Some v), state)
    | Stmt.Block block -> Cont (block @ List.tl stmts), state
    | _ -> Cont (List.tl stmts), state
  in { c with code = code'; state = state' }

let rec eval (c : config) : config =
  match c.code with
  | Cont [] -> raise (Runtime_error "Empty stmts list")
  | Cont stmts -> eval (step c)
  | Error v -> (
      match v with
      | Some v' -> raise (Runtime_error ("Error: " ^ Sval.str v'))
      | None -> raise (Runtime_error "Unknown runtime error"))
  | Final v -> c

let invoke (prog : Prog.t) (f : func) : config =
  let func = Prog.get_func prog f in
  let heap = Heap.create ()
  and store = Sstore.create []
  and stack = Call_stack.push Call_stack.empty Call_stack.Toplevel in
  let state = (heap, store, stack, f) in
  eval { prog = prog; code = Cont [ func.body ]; state = state }

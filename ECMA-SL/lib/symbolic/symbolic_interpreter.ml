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

type state = Heap.t * Store.t * Call_stack.t * func

type outcome =
  | Cont of Stmt.t list
  | Error of Val.t option
  | Final of Val.t option

type config = {
  prog : Prog.t;
  code : outcome;
  state : state;
}

exception Runtime_error of string

let step (c : config) : config =
  let { prog = prog; code = code; state = state } = c in
  (*let heap, store, stack, f = state in*)
  let stmts = match code with Cont stmts -> stmts | _ -> failwith "Empty Cont" in
  let s = List.hd stmts in
  let code', state' = match s with
    | Stmt.Skip -> Cont (List.tl stmts), state
    | Stmt.Merge -> Cont (List.tl stmts), state
    | Stmt.Exception err -> Error (Some (Val.Str err)), state
    | _ -> Cont (List.tl stmts), state
  in { c with code = code'; state = state' }

let rec eval (c : config) : config =
  match c.code with
  | Cont [] -> raise (Runtime_error "Empty stmts list")
  | Cont stmts -> eval (step c)
  | Error v -> (
      match v with
      | Some v' -> raise (Runtime_error ("Error: " ^ Val.str v'))
      | None -> raise (Runtime_error "Unknown runtime error"))
  | Final v -> c

let invoke (prog : Prog.t) (f : func) : config =
  let func = Prog.get_func prog f in
  let heap = Heap.create ()
  and store = Store.create []
  and stack = Call_stack.push Call_stack.empty Call_stack.Toplevel in
  let state = (heap, store, stack, f) in
  eval { prog = prog; code = Cont [ func.body ]; state = state }

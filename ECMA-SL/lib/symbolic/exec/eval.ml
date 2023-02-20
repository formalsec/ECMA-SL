open Func
open Source

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
  | Failure of Sval.t option

(* TODO:
   | Unknwon of Sval.t option
*)
and func = string
and stack = Sstore.t Call_stack.t
and state = Sval.t Heap.t * Sstore.t * stack * func
and pc = Sval.t list

exception Runtime_error of string

let rte (msg : string) = raise (Runtime_error msg)
let is_fail (o : outcome) : bool = match o with Failure _ -> true | _ -> false
let is_final (o : outcome) : bool = match o with Final _ -> true | _ -> false
let update (c : config) code state pc : config = { c with code; state; pc }

let loc (v : Sval.t) : string =
  match v with Sval.Loc l -> l | _ -> rte "Invalid location!"

let field (v : Sval.t) : string =
  match v with Sval.Str f -> f | _ -> rte "Invalid field!"

let func (v : Sval.t) : string * Sval.t list =
  match v with
  | Sval.Str x -> (x, [])
  | Sval.Curry (x, vs) -> (x, vs)
  | _ -> rte "Wrong/Invalid function identifier"

let rec eval_expression (store : Sstore.t) (e : Expr.t) : Sval.t =
  match e with
  | Expr.Val n -> Sval.of_val n
  | Expr.Var x -> (
      match Sstore.find_opt store x with
      | Some v -> v
      | None -> rte ("Cannot find var '" ^ x ^ "'"))
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
  | Expr.Curry (f, es) -> (
      let f' = eval_expression store f
      and vs = List.map (eval_expression store) es in
      match f' with
      | Sval.Str s -> Sval.Curry (s, vs)
      | _ -> rte "eval_expression: Illegal 'Curry' expresion!")
  | Expr.Symbolic (t, x) ->
      let x' =
        match eval_expression store x with
        | Sval.Str s -> s
        | _ -> rte "invalid symbolic variable name"
      in
      Sval.Symbolic (t, x')

let step (c : config) : config list =
  let { prog; code; state; pc; solver } = c in
  let heap, store, stack, f = state in
  let stmts =
    match code with Cont stmts -> stmts | _ -> rte "step: Empty continuation!"
  in
  let s = List.hd stmts in
  match s.it with
  | Stmt.Skip -> [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Merge -> [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Exception err ->
      print_endline (Source.string_of_region s.at ^ ": Exception: " ^ err);
      [ update c (Error (Some (Sval.Str err))) state pc ]
  | Stmt.Print e ->
      print_endline (Expr.str e);
      [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Fail e ->
      [ update c (Error (Some (eval_expression store e))) state pc ]
  | Stmt.Abort e ->
      [ update c (Final (Some (eval_expression store e))) state pc ]
  | Stmt.Assume e when Sval.Bool true = eval_expression store e ->
      [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Assume e when Sval.Bool false = eval_expression store e -> []
  | Stmt.Assume e ->
      let pc' = eval_expression store e :: pc in
      if not (Encoding.check solver pc') then []
      else [ update c (Cont (List.tl stmts)) state pc' ]
  | Stmt.Assert e when Sval.Bool true = eval_expression store e ->
      [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Assert e when Sval.Bool false = eval_expression store e ->
      [ update c (Failure (Some (eval_expression store e))) state pc ]
  | Stmt.Assert e ->
      let v = eval_expression store e in
      let v' = eval_expression store (Expr.UnOpt (Operators.Not, e)) in
      if Encoding.check solver (v' :: pc) then
        [ update c (Failure (Some v)) state pc ]
      else [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.Assign (x, e) ->
      let v = eval_expression store e in
      [
        update c
          (Cont (List.tl stmts))
          (heap, Sstore.add store x v, stack, f)
          pc;
      ]
  | Stmt.Block blk -> [ update c (Cont (blk @ List.tl stmts)) state pc ]
  | Stmt.If (br, blk, _) when Sval.Bool true = eval_expression store br ->
      let cont =
        match blk.it with
        | Stmt.Block b -> b @ ((Stmt.Merge @@ blk.at) :: List.tl stmts)
        | _ -> rte "Malformed if statement 'then' block!"
      in
      [ update c (Cont cont) state pc ]
  | Stmt.If (br, _, blk) when Sval.Bool false = eval_expression store br ->
      let cont =
        let t = List.tl stmts in
        match blk with None -> t | Some s' -> s' :: (Stmt.Merge @@ s'.at) :: t
      in
      [ update c (Cont cont) state pc ]
  | Stmt.If (br, blk1, blk2) ->
      let br_t = eval_expression store br
      and br_f = eval_expression store (Expr.UnOpt (Operators.Not, br)) in
      let then_branch =
        if not (Encoding.check solver (br_t :: pc)) then []
        else
          let stmts' = blk1 :: (Stmt.Merge @@ blk1.at) :: List.tl stmts in
          [ update c (Cont stmts') state (br_t :: pc) ]
      in
      let else_branch =
        if not (Encoding.check solver (br_f :: pc)) then []
        else
          let state' = (Heap.clone heap, store, stack, f) in
          let stmts' =
            match blk2 with
            | None -> List.tl stmts
            | Some s' -> s' :: (Stmt.Merge @@ s'.at) :: List.tl stmts
          in
          [ update c (Cont stmts') state' (br_f :: pc) ]
      in
      then_branch @ else_branch
  | Stmt.While (br, blk) ->
      let blk' =
        Stmt.Block (blk :: [ Stmt.While (br, blk) @@ s.at ]) @@ blk.at
      in
      [
        update c
          (Cont ((Stmt.If (br, blk', None) @@ s.at) :: List.tl stmts))
          state pc;
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
  | Stmt.AssignCall (x, e, es) ->
      let f', vs = func (eval_expression store e) in
      let vs' = vs @ List.map (eval_expression store) es in
      let func = Prog.get_func prog f' in
      let stack' =
        Call_stack.push stack
          (Call_stack.Intermediate (List.tl stmts, store, x, f))
      in
      let store' = Sstore.create (List.combine (Prog.get_params prog f') vs') in
      [ update c (Cont [ func.body ]) (heap, store', stack', f') pc ]
  | Stmt.AssignECall (x, y, es) ->
      (* TODO: *) rte "Eval: step: 'AssignECall' not implemented!"
  | Stmt.AssignNewObj x ->
      let obj = Object.create () in
      let loc = Heap.insert heap obj in
      [
        update c
          (Cont (List.tl stmts))
          (heap, Sstore.add store x (Sval.Loc loc), stack, f)
          pc;
      ]
  | Stmt.AssignInObjCheck (x, e_field, e_loc) ->
      let loc = loc (eval_expression store e_loc)
      and field = field (eval_expression store e_field) in
      let v = Sval.Bool (Option.is_some (Heap.get_field heap loc field)) in
      [
        update c
          (Cont (List.tl stmts))
          (heap, Sstore.add store x v, stack, f)
          pc;
      ]
  | Stmt.AssignObjToList (x, e) ->
      let loc = loc (eval_expression store e) in
      let v =
        match Heap.get heap loc with
        | None -> rte ("AssignObjToList: '" ^ loc ^ "' not found in heap")
        | Some obj ->
            Sval.List
              (List.map
                 (fun (f, v) -> Sval.(Tuple (Str f :: [ v ])))
                 (Object.to_list obj))
      in
      [
        update c
          (Cont (List.tl stmts))
          (heap, Sstore.add store x v, stack, f)
          pc;
      ]
  | Stmt.AssignObjFields (x, e) ->
      let loc = loc (eval_expression store e) in
      let v =
        match Heap.get heap loc with
        | None -> rte ("AssignObjFields: '" ^ loc ^ "' not found in heap")
        | Some obj ->
            Sval.List (List.map (fun f -> Sval.Str f) (Object.get_fields obj))
      in
      [
        update c
          (Cont (List.tl stmts))
          (heap, Sstore.add store x v, stack, f)
          pc;
      ]
  | Stmt.FieldAssign (e_loc, e_field, e_v) ->
      let loc = loc (eval_expression store e_loc)
      and field = field (eval_expression store e_field)
      and v = eval_expression store e_v in
      Heap.set_field heap loc field v;
      [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.FieldDelete (e_loc, e_field) ->
      let loc = loc (eval_expression store e_loc)
      and field = field (eval_expression store e_field) in
      Heap.delete_field heap loc field;
      [ update c (Cont (List.tl stmts)) state pc ]
  | Stmt.FieldLookup (x, e_loc, e_field) ->
      let loc = loc (eval_expression store e_loc)
      and field = field (eval_expression store e_field) in
      let v =
        Option.default (Sval.Symbol "undefined") (Heap.get_field heap loc field)
      in
      [
        update c
          (Cont (List.tl stmts))
          (heap, Sstore.add store x v, stack, f)
          pc;
      ]

(* Credit: Thanks to Joao Borges for writing this code *)
module type Work_list = sig
  type 'a t

  exception Empty

  val create : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val pop : 'a t -> 'a
  val is_empty : 'a t -> bool
end

(* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
module Strategy (L : Work_list) = struct
  let eval c : config list =
    let w = L.create () in
    L.push c w;
    let out = ref [] and err = ref false in
    while not (!err || L.is_empty w) do
      let c = L.pop w in
      match c.code with
      | Cont [] -> rte "eval: Empty continuation!"
      | Cont _ -> List.iter (fun c -> L.push c w) (step c)
      | Error v -> out := c :: !out
      | Final v -> out := c :: !out
      | Failure v ->
          err := true;
          out := c :: !out
    done;
    !out
end

module RandArray : Work_list = struct
  type 'a t = 'a BatDynArray.t

  exception Empty

  let create () = BatDynArray.create ()
  let is_empty a = BatDynArray.empty a
  let push v a = BatDynArray.add a v

  let pop a =
    let i = Random.int (BatDynArray.length a) in
    let v = BatDynArray.get a i in
    BatDynArray.delete a i;
    v
end

module DFS = Strategy (Stack)
module BFS = Strategy (Queue)
module RND = Strategy (RandArray)

let invoke (prog : Prog.t) (f : func) (eval : config -> config list) :
    config list =
  let func = Prog.get_func prog f in
  let heap = Heap.create ()
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
  eval initial_config

let analyse (prog : Prog.t) (f : func) : Report.t =
  let time_analysis = ref 0.0 in
  let eval =
    match !Flags.policy with
    | "breadth" -> BFS.eval
    | "depth" -> DFS.eval
    | "random" -> RND.eval
    | _ -> rte ("Invalid search policy '" ^ !Flags.policy ^ "'")
  in
  let out_configs =
    Time_utils.time_call time_analysis (fun () -> invoke prog f eval)
  in
  let final_configs = List.filter (fun c -> is_final c.code) out_configs
  and error_configs = List.filter (fun c -> is_fail c.code) out_configs in
  let final_testsuite, error_testsuite =
    ( List.map (fun c -> Encoding.model c.solver c.pc) final_configs,
      List.map (fun c -> Encoding.model c.solver c.pc) error_configs )
  in
  let report =
    Report.create !Flags.file (List.length out_configs)
      (List.length error_configs)
      0 !time_analysis !Encoding.time_solver
  in
  Report.add_testsuites report final_testsuite error_testsuite

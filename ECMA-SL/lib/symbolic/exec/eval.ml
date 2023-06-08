open Core
open Encoding
open Expr
open Func
open State
open Source
open Reducer
module Crash = Err.Make ()
module Invalid_arg = Err.Make ()

exception Crash = Crash.Error
exception Invalid_arg = Invalid_arg.Error

let loc (at : region) (e : Expr.t) : string =
  match e with
  | Val (Val.Loc l) -> l
  | _ ->
      Invalid_arg.error at ("Expr '" ^ Expr.str e ^ "' is not a loc expression")

(* let field (at : region) (v : Expr.t) : string =
   match v with
   | Val (Val.Str f) -> f
   | _ ->
       print_endline (Expr.str v);
       Invalid_arg.error at "Sval is not a 'field' expression" *)

let func (at : region) (v : Expr.t) : string * Expr.t list =
  match v with
  | Val (Val.Str x) -> (x, [])
  | Curry (Val (Val.Str x), vs) -> (x, vs)
  | _ -> Invalid_arg.error at "Sval is not a 'func' identifier"

let eval_api_call ?(at = no_region) (store : Sstore.t) (c : config)
    (st : Symb_stmt.t) : Sstore.t =
  match st with
  | Symb_stmt.IsSymbolic (name, e) ->
      let e' = reduce_expr ~at store e in
      Sstore.add_exn store name (Val (Val.Bool (Expr.is_symbolic e')))
  | Symb_stmt.IsNumber (name, e) ->
      let e' = reduce_expr ~at store e in
      let tp = Sval_typing.type_of e' in
      let result =
        match tp with
        | Some Type.IntType | Some Type.FltType -> true
        | _ -> false
      in
      Sstore.add_exn store name (Val (Val.Bool result))
  | Symb_stmt.IsSat (name, e) ->
      let e' = Translator.translate (reduce_expr ~at store e) in
      let sat = Batch.check_sat c.solver (e' :: c.pc) in
      Sstore.add_exn store name (Val (Val.Bool sat))
  | Symb_stmt.Maximize (name, e) ->
      let e' = Translator.translate (reduce_expr ~at store e) in
      let v =
        Option.map ~f:Translator.expr_of_value
          (Optimizer.maximize c.opt e' c.pc)
      in
      Sstore.add_exn store name (Option.value ~default:(Val Val.Null) v)
  | Symb_stmt.Minimize (name, e) ->
      let e' = Translator.translate (reduce_expr ~at store e) in
      let v =
        Option.map ~f:Translator.expr_of_value
          (Optimizer.minimize c.opt e' c.pc)
      in
      Sstore.add_exn store name (Option.value ~default:(Val Val.Null) v)
  | Symb_stmt.Eval (name, e) ->
      let e' = Translator.translate (reduce_expr ~at store e) in
      let v =
        Option.map ~f:Translator.expr_of_value (Batch.eval c.solver e' c.pc)
      in
      Sstore.add_exn store name (Option.value ~default:(Val Val.Null) v)

let step (c : config) : config list =
  let open Stmt in
  let { prog; code; state; pc; solver; opt } = c in
  let heap, store, stack, f = state in
  let stmts =
    match code with
    | Cont stmts -> stmts
    | _ -> Crash.error no_region "step: Empty continuation!"
  in
  let s = List.hd_exn stmts in
  match s.it with
  | Skip -> [ update c (Cont (List.tl_exn stmts)) state pc ]
  | Merge -> [ update c (Cont (List.tl_exn stmts)) state pc ]
  | Exception err ->
      fprintf stderr "%s: Exception: %s\n" (Source.string_of_region s.at) err;
      [ update c (Error (Some (Val (Val.Str err)))) state pc ]
  | Print e ->
      Logging.print_endline (lazy (Expr.str (reduce_expr ~at:s.at store e)));
      [ update c (Cont (List.tl_exn stmts)) state pc ]
  | Fail e ->
      [ update c (Error (Some (reduce_expr ~at:s.at store e))) state pc ]
  | Abort e ->
      [ update c (Final (Some (reduce_expr ~at:s.at store e))) state pc ]
  | Assume e
    when Expr.equal (Val (Val.Bool true)) (reduce_expr ~at:s.at store e) ->
      [ update c (Cont (List.tl_exn stmts)) state pc ]
  | Assume e
    when Expr.equal (Val (Val.Bool false)) (reduce_expr ~at:s.at store e) ->
      []
  | Assume e ->
      let e' = reduce_expr ~at:s.at store e in
      let v = Translator.translate e' in
      let cont =
        if not (Batch.check_sat solver (v :: pc)) then []
        else [ update c (Cont (List.tl_exn stmts)) state (v :: pc) ]
      in
      Logging.print_endline
        (lazy
          ("assume (" ^ Expr.str e' ^ ") = "
          ^ Bool.to_string (List.length cont > 0)));
      cont
  | Stmt.Assert e
    when Expr.equal (Val (Val.Bool true)) (reduce_expr ~at:s.at store e) ->
      [ update c (Cont (List.tl_exn stmts)) state pc ]
  | Stmt.Assert e
    when Expr.equal (Val (Val.Bool false)) (reduce_expr ~at:s.at store e) ->
      printf "%s = %s\n" (Expr.str e) (Expr.str (reduce_expr ~at:s.at store e));
      [ update c (Failure (Some (reduce_expr ~at:s.at store e))) state pc ]
  | Stmt.Assert e ->
      let v = reduce_expr ~at:s.at store e in
      let v' = reduce_expr ~at:s.at store (Expr.UnOpt (Operators.Not, e)) in
      let cont =
        if Batch.check_sat solver (Translator.translate v' :: pc) then
          [ update c (Failure (Some v)) state pc ]
        else [ update c (Cont (List.tl_exn stmts)) state pc ]
      in
      Logging.print_endline
        (lazy
          ("assert (" ^ Expr.str v ^ ") = " ^ Bool.to_string (is_cont c.code)));
      cont
  | Stmt.Assign (x, e) ->
      let v = reduce_expr ~at:s.at store e in
      [
        update c
          (Cont (List.tl_exn stmts))
          (heap, Sstore.add_exn store x v, stack, f)
          pc;
      ]
  | Stmt.Block blk -> [ update c (Cont (blk @ List.tl_exn stmts)) state pc ]
  | Stmt.If (br, blk, _)
    when Expr.equal (Val (Val.Bool true)) (reduce_expr ~at:s.at store br) ->
      let cont =
        match blk.it with
        | Stmt.Block b -> b @ ((Stmt.Merge @@ blk.at) :: List.tl_exn stmts)
        | _ -> Crash.error s.at "Malformed if statement 'then' block!"
      in
      [ update c (Cont cont) state pc ]
  | Stmt.If (br, _, blk)
    when Expr.equal (Val (Val.Bool false)) (reduce_expr ~at:s.at store br) ->
      let cont =
        let t = List.tl_exn stmts in
        match blk with None -> t | Some s' -> s' :: (Stmt.Merge @@ s'.at) :: t
      in
      [ update c (Cont cont) state pc ]
  | Stmt.If (br, blk1, blk2) ->
      let br_t = reduce_expr ~at:s.at store br
      and br_f = reduce_expr ~at:s.at store (Expr.UnOpt (Operators.Not, br)) in
      Logging.print_endline
        (lazy
          (sprintf "%s: If (%s)" (Source.string_of_region s.at) (Expr.str br_t)));
      let br_t' = Translator.translate br_t
      and br_f' = Translator.translate br_f in
      let then_branch =
        try
          if not (Batch.check_sat solver (br_t' :: pc)) then []
          else
            let stmts' = blk1 :: (Stmt.Merge @@ blk1.at) :: List.tl_exn stmts in
            [ update c (Cont stmts') state (br_t' :: pc) ]
        with Batch.Unknown ->
          [ update c (Unknown (Some br_t)) state (br_t' :: pc) ]
      in
      let else_branch =
        try
          if not (Batch.check_sat solver (br_f' :: pc)) then []
          else
            let state' = (S_heap.clone heap, store, stack, f) in
            let stmts' =
              match blk2 with
              | None -> List.tl_exn stmts
              | Some s' -> s' :: (Stmt.Merge @@ s'.at) :: List.tl_exn stmts
            in
            [ update c (Cont stmts') state' (br_f' :: pc) ]
        with Batch.Unknown ->
          [ update c (Unknown (Some br_f)) state (br_f' :: pc) ]
      in
      let branches = then_branch @ else_branch in 
      branches
  | Stmt.While (br, blk) ->
      let blk' =
        Stmt.Block (blk :: [ Stmt.While (br, blk) @@ s.at ]) @@ blk.at
      in
      [
        update c
          (Cont ((Stmt.If (br, blk', None) @@ s.at) :: List.tl_exn stmts))
          state pc;
      ]
  | Stmt.Return e -> (
      let v = reduce_expr ~at:s.at store e in
      let frame, stack' = Call_stack.pop stack in
      match frame with
      | Call_stack.Intermediate (stmts', store', x, f') ->
          [
            update c (Cont stmts')
              (heap, Sstore.add_exn store' x v, stack', f')
              pc;
          ]
      | Call_stack.Toplevel -> [ update c (Final (Some v)) state pc ])
  | Stmt.AssignCall (x, e, es) ->
      let f', vs = func s.at (reduce_expr ~at:s.at store e) in
      let vs' = vs @ List.map ~f:(reduce_expr ~at:s.at store) es in
      let func = Prog.get_func prog f' in
      let stack' =
        Call_stack.push stack
          (Call_stack.Intermediate (List.tl_exn stmts, store, x, f))
      in
      let store' = Sstore.create (List.zip_exn (Prog.get_params prog f') vs') in
      [ update c (Cont [ func.body ]) (heap, store', stack', f') pc ]
  | Stmt.AssignECall (x, y, es) ->
      Crash.error s.at "'AssignECall' not implemented!"
  | Stmt.AssignNewObj x ->
      let obj = S_object.create () in
      let loc = S_heap.insert heap obj in
      [
        update c
          (Cont (List.tl_exn stmts))
          (heap, Sstore.add_exn store x (Val (Val.Loc loc)), stack, f)
          pc;
      ]
  | Stmt.AssignInObjCheck (x, e_field, e_loc) ->
      let loc = loc s.at (reduce_expr ~at:s.at store e_loc) in
      let reduced_field = reduce_expr ~at:s.at store e_field in
      let get_result = S_heap.get_field heap loc reduced_field solver pc store in

      List.map get_result ~f:(fun (new_heap, obj, new_pc, v) ->
          let v' = Val (Val.Bool (Option.is_some v)) in
          let new_pc' = match new_pc with 
            | Some p -> [ p ] 
            | None -> [] in

          let new_pc' = new_pc' @ pc in
  
          update c
            (Cont (List.tl_exn stmts))
            (new_heap, Sstore.add_exn store x v', stack, f)
            (new_pc'))
  | Stmt.AssignObjToList (x, e) ->
      let loc = loc s.at (reduce_expr ~at:s.at store e) in
      let v =
        match S_heap.get heap loc with
        | None -> Crash.error s.at ("'" ^ loc ^ "' not found in heap")
        | Some obj ->
            NOpt
              ( Operators.ListExpr,
                List.map
                  ~f:(fun (f, v) ->
                    NOpt (Operators.TupleExpr, Val (Val.Str f) :: [ v ]))
                  (S_object.to_list obj) )
      in
      [
        update c
          (Cont (List.tl_exn stmts))
          (heap, Sstore.add_exn store x v, stack, f)
          pc;
      ]
  | Stmt.AssignObjFields (x, e) ->
      let loc = loc s.at (reduce_expr ~at:s.at store e) in
      let v =
        match S_heap.get heap loc with
        | None -> Crash.error s.at ("'" ^ loc ^ "' not found in heap")
        | Some obj -> NOpt (Operators.ListExpr, S_object.get_fields obj)
      in
      [
        update c
          (Cont (List.tl_exn stmts))
          (heap, Sstore.add_exn store x v, stack, f)
          pc;
      ]
  | Stmt.FieldAssign (e_loc, e_field, e_v) ->
      let loc = loc s.at (reduce_expr ~at:s.at store e_loc)
      and reduced_field = reduce_expr ~at:s.at store e_field
      and v = reduce_expr ~at:s.at store e_v in

      let objects = S_heap.set_field heap loc reduced_field v solver pc store in
      List.map objects ~f:(fun (new_heap, obj, new_pc) ->
          let new_pc' = match new_pc with Some p -> [ p ] | None -> [] in
          update c
            (Cont (List.tl_exn stmts))
            (new_heap, store, stack, f)
            (new_pc' @ pc))
  | Stmt.FieldDelete (e_loc, e_field) ->
      let loc = loc s.at (reduce_expr ~at:s.at store e_loc) in
      let reduced_field = reduce_expr ~at:s.at store e_field in
      let objects = S_heap.delete_field heap loc reduced_field solver pc store in

      List.map objects ~f:(fun (new_heap, obj, new_pc) ->
          let new_pc' = match new_pc with Some p -> [ p ] | None -> [] in
          update c
            (Cont (List.tl_exn stmts))
            (new_heap, store, stack, f)
            (new_pc' @ pc))
  | Stmt.FieldLookup (x, e_loc, e_field) ->
      let loc = loc s.at (reduce_expr ~at:s.at store e_loc)
      and reduced_field = reduce_expr ~at:s.at store e_field in
      let objects = S_heap.get_field heap loc reduced_field solver pc store in

      List.map objects ~f:(fun (new_heap, obj, new_pc, v) ->
          let new_pc' = match new_pc with Some p -> [ p ] | None -> [] in
          let v' =
            match v with Some v -> v | None -> Val (Val.Symbol "undefined")
          in
          update c
            (Cont (List.tl_exn stmts))
            (new_heap, Sstore.add_exn store x v', stack, f)
            (new_pc' @ pc))
  | Stmt.SymbStmt symb_s ->
      let store' = eval_api_call ~at:s.at store c symb_s in
      [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]

module type WorkList = sig
  type 'a t

  exception Empty

  val create : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val pop : 'a t -> 'a
  val is_empty : 'a t -> bool
end

(* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
module TreeSearch (L : WorkList) = struct
  let eval c : config list =
    let w = L.create () in
    L.push c w;
    let out = ref [] and err = ref false in
    Logging.print_endline (lazy "---- Start ----");
    while not (!err || L.is_empty w) do
      let c = L.pop w in
      match c.code with
      | Cont [] -> Crash.error Source.no_region "eval: Empty continuation!"
      | Cont _ -> List.iter ~f:(fun c -> L.push c w) (step c)
      | Error v | Final v | Unknown v ->
          if L.is_empty w then Logging.print_endline (lazy "---- End ---- ");
          out := c :: !out
      | Failure v ->
          Logging.print_endline (lazy "---- Failure ----");
          err := true;
          out := c :: !out
    done;
    !out
end

module RandArray : WorkList = struct
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

module DFS = TreeSearch (Caml.Stack)
module BFS = TreeSearch (Caml.Queue)
module RND = TreeSearch (RandArray)

(* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
let invoke (prog : Prog.t) (func : Func.t) (eval : config -> config list) :
    config list =
  let heap = S_heap.create ()
  and store = Sstore.create []
  and stack = Call_stack.push Call_stack.empty Call_stack.Toplevel in
  let solver =
    let s = Batch.create () in
    if !Config.axioms then Batch.set_default_axioms s;
    s
  in
  let initial_config =
    {
      prog;
      code = Cont [ func.body ];
      state = (heap, store, stack, func.name);
      pc = [];
      solver;
      opt = Optimizer.create ();
    }
  in
  eval initial_config

let analyse (prog : Prog.t) (f : func) : Report.t =
  let time_analysis = ref 0.0 in
  let f = Prog.get_func prog f in
  let eval =
    match !Config.policy with
    | "breadth" -> BFS.eval
    | "depth" -> DFS.eval
    | "random" -> RND.eval
    | _ ->
        Crash.error f.body.at ("Invalid search policy '" ^ !Config.policy ^ "'")
  in
  let out_configs =
    Time_utils.time_call time_analysis (fun () -> invoke prog f eval)
  in
  let final_configs = List.filter ~f:(fun c -> is_final c.code) out_configs
  and error_configs = List.filter ~f:(fun c -> is_fail c.code) out_configs in
  let final_testsuite, error_testsuite =
    let f c =
      ignore (Batch.check_sat c.solver c.pc);
      let symbols = Expression.get_symbols c.pc in
      (* List.iter c.pc ~f:( fun cond -> Printf.printf "PC: %s\n" (Expression.to_string cond));
      Printf.printf"\n"; *)
      List.map (Batch.value_binds ~symbols c.solver) ~f:(fun (s, v) ->
          ( "NA",
            Encoding.Symbol.to_string s,
            Expression.to_string (Expression.Val v) ))
    in
    (List.map ~f final_configs, List.map ~f error_configs)
  in
  let report =
    Report.create ~file:!Config.file ~paths:(List.length out_configs)
      ~errors:(List.length error_configs)
      ~unknowns:0 ~analysis:!time_analysis ~solver:!Batch.solver_time
  in
  Report.add_testsuites report ~final:final_testsuite ~error:error_testsuite

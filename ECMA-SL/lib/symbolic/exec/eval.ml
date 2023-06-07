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
      Invalid_arg.error at "Expr '" ^ Expr.str e ^ "' is not a loc expression"

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
      [
        update c
          (Failure ("Abort", Some (reduce_expr ~at:s.at store e)))
          state pc;
      ]
  | Stmt.Assign (x, e) ->
      let v = reduce_expr ~at:s.at store e in
      [
        update c
          (Cont (List.tl_exn stmts))
          (heap, Sstore.add_exn store x v, stack, f)
          pc;
      ]
  | Stmt.Assert e
    when Expr.equal (Val (Val.Bool true)) (reduce_expr ~at:s.at store e) ->
      [ update c (Cont (List.tl_exn stmts)) state pc ]
  | Stmt.Assert e
    when Expr.equal (Val (Val.Bool false)) (reduce_expr ~at:s.at store e) ->
      printf "%s" (Encoding.Expression.string_of_pc pc);
      printf "%s = %s\n" (Expr.str e) (Expr.str (reduce_expr ~at:s.at store e));
      let e' = Some (reduce_expr ~at:s.at store e) in
      [ update c (Failure ("assert", e')) state pc ]
  | Stmt.Assert e ->
      let v = reduce_expr ~at:s.at store e in
      let v' = reduce_expr ~at:s.at store (Expr.UnOpt (Operators.Not, e)) in
      let cont =
        if Batch.check_sat solver (Translator.translate v' :: pc) then
          [ update c (Failure ("assert", Some v)) state pc ]
        else [ update c (Cont (List.tl_exn stmts)) state pc ]
      in
      Logging.print_endline
        (lazy
          ("assert (" ^ Expr.str v ^ ") = " ^ Bool.to_string (is_cont c.code)));
      cont
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
      else_branch @ then_branch
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
      let get_result = S_heap.get_field heap loc reduced_field solver pc in

      if List.length get_result > 1 then
        Printf.printf "branching in assignInObjCheck\n";

      List.map get_result ~f:(fun (new_heap, obj, new_pc, v) ->
          let v' = Val (Val.Bool (Option.is_some v)) in
          let new_pc' = match new_pc with Some p -> [ p ] | None -> [] in

          let new_pc' = new_pc' @ pc in
          (* if List.length get_result > 1 then(
               List.iter new_pc' ~f:(fun v -> Printf.printf "----%s\n" (Encoding.Expression.to_string v));
               Printf.printf "\n";
             ); *)
          update c
            (Cont (List.tl_exn stmts))
            (new_heap, Sstore.add_exn store x v', stack, f)
            new_pc')
      (* let field = field s.at reduced_field in
         let v = Val (Val.Bool (Option.is_some (Heap.get_field heap loc field))) in *)
      (* [
           update c
             (Cont (List.tl_exn stmts))
             (heap, Sstore.add_exn store x v, stack, f)
             pc;
         ] *)
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
      let objects = S_heap.set_field heap loc reduced_field v solver pc in

      if List.length objects > 1 then Printf.printf "branching on assign\n";
      List.map objects ~f:(fun (new_heap, obj, new_pc) ->
          let new_pc' = match new_pc with Some p -> [ p ] | None -> [] in
          update c
            (Cont (List.tl_exn stmts))
            (new_heap, store, stack, f)
            (new_pc' @ pc))
      (* [ update c (Cont (List.tl_exn stmts)) state pc ] *)
  | Stmt.FieldDelete (e_loc, e_field) ->
      let loc = loc s.at (reduce_expr ~at:s.at store e_loc) in
      let reduced_field = reduce_expr ~at:s.at store e_field in
      let objects = S_heap.delete_field heap loc reduced_field solver pc in

      if List.length objects > 1 then Printf.printf "branching on delete\n";
      List.map objects ~f:(fun (new_heap, obj, new_pc) ->
          let new_pc' = match new_pc with Some p -> [ p ] | None -> [] in
          update c
            (Cont (List.tl_exn stmts))
            (new_heap, store, stack, f)
            (new_pc' @ pc))
      (* [ update c (Cont (List.tl_exn stmts)) state pc ] *)
  | Stmt.FieldLookup (x, e_loc, e_field) ->
      let loc = loc s.at (reduce_expr ~at:s.at store e_loc)
      and reduced_field = reduce_expr ~at:s.at store e_field in
      let objects = S_heap.get_field heap loc reduced_field solver pc in

      if List.length objects > 1 then Printf.printf "branching on lookup\n";

      List.map objects ~f:(fun (new_heap, obj, new_pc, v) ->
          let new_pc' = match new_pc with Some p -> [ p ] | None -> [] in
          let v' =
            match v with Some v -> v | None -> Val (Val.Symbol "undefined")
          in
          update c
            (Cont (List.tl_exn stmts))
            (new_heap, Sstore.add_exn store x v', stack, f)
            (new_pc' @ pc))
      (* let v =
           Option.value ~default:(Val (Val.Symbol "undefined"))
             (Heap.get_field heap loc field)
         in
         [
           update c
             (Cont (List.tl_exn stmts))
             (heap, Sstore.add_exn store x v, stack, f)
             pc;
         ] *)
  | Stmt.SymStmt (SymStmt.Assume e)
    when Expr.equal (Val (Val.Bool true)) (reduce_expr ~at:s.at store e) ->
      [ update c (Cont (List.tl_exn stmts)) state pc ]
  | Stmt.SymStmt (SymStmt.Assume e)
    when Expr.equal (Val (Val.Bool false)) (reduce_expr ~at:s.at store e) ->
      []
  | Stmt.SymStmt (SymStmt.Assume e) ->
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
  | Stmt.SymStmt (SymStmt.Evaluate (x, e)) ->
      let e' = Translator.translate (reduce_expr ~at:s.at store e) in
      let v =
        Option.map ~f:Translator.expr_of_value (Batch.eval c.solver e' c.pc)
      in
      let store' =
        Sstore.add_exn store x (Option.value ~default:(Val Val.Null) v)
      in
      [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
  | Stmt.SymStmt (SymStmt.Maximize (x, e)) ->
      let e' = Translator.translate (reduce_expr ~at:s.at store e) in
      let v =
        Option.map ~f:Translator.expr_of_value
          (Optimizer.maximize c.opt e' c.pc)
      in
      let store' =
        Sstore.add_exn store x (Option.value ~default:(Val Val.Null) v)
      in
      [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
  | Stmt.SymStmt (SymStmt.Minimize (x, e)) ->
      let e' = Translator.translate (reduce_expr ~at:s.at store e) in
      let v =
        Option.map ~f:Translator.expr_of_value
          (Optimizer.minimize c.opt e' c.pc)
      in
      let store' =
        Sstore.add_exn store x (Option.value ~default:(Val Val.Null) v)
      in
      [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
  | Stmt.SymStmt (SymStmt.Is_symbolic (x, e)) ->
      let e' = reduce_expr ~at:s.at store e in
      let store' =
        Sstore.add_exn store x (Val (Val.Bool (Expr.is_symbolic e')))
      in
      [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
  | Stmt.SymStmt (SymStmt.Is_sat (x, e)) ->
      let e' = Translator.translate (reduce_expr ~at:s.at store e) in
      let sat = Batch.check_sat c.solver (e' :: c.pc) in
      let store' = Sstore.add_exn store x (Val (Val.Bool sat)) in
      [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
  | Stmt.SymStmt (SymStmt.Is_number (x, e)) ->
      let e' = reduce_expr ~at:s.at store e in
      let is_num =
        match Sval_typing.type_of e' with
        | Some Type.IntType | Some Type.FltType -> true
        | _ -> false
      in
      let store' = Sstore.add_exn store x (Val (Val.Bool is_num)) in
      [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]

module type WorkList = sig
  type 'a t

  exception Empty

  val create : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val pop : 'a t -> 'a
  val is_empty : 'a t -> bool
  val length : 'a t -> int
end

(* Source: Thanks to Joao Borges (@RageKnify) for writing this code *)
module TreeSearch (L : WorkList) = struct
  let eval c : config list =
    let w = L.create () in
    L.push c w;
    let out = ref [] in
    while not (L.is_empty w) do
      let c = L.pop w in
      match c.code with
      | Cont [] -> Crash.error Source.no_region "eval: Empty continuation!"
      | Cont _ -> List.iter ~f:(fun c -> L.push c w) (step c)
      | Error v | Final v | Unknown v -> out := c :: !out
      | Failure (f, e) ->
          let e' = Option.value_map e ~default:"" ~f:Expr.str in
          Logging.print_endline (lazy (sprintf "Failure: %s: %s" f e'));
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

  let length = BatDynArray.length
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

let analyse (prog : Prog.t) (f : func) (policy : string) : config list =
  let f = Prog.get_func prog f in
  let eval =
    match policy with
    | "breadth" -> BFS.eval
    | "depth" -> DFS.eval
    | "random" -> RND.eval
    | _ ->
        Crash.error f.body.at ("Invalid search policy '" ^ !Config.policy ^ "'")
  in
  invoke prog f eval

let main (prog : Prog.t) (f : func) : unit =
  let time_analysis = ref 0.0 in
  let configs =
    Time_utils.time_call time_analysis (fun () -> analyse prog f !Config.policy)
  in
  let testsuite_path = Filename.concat !Config.workspace "test-suite" in
  Io.safe_mkdir testsuite_path;
  let final_configs = List.filter ~f:(fun c -> is_final c.code) configs
  and error_configs = List.filter ~f:(fun c -> is_fail c.code) configs in
  let f c =
    let open Encoding in
    let testcase =
      List.map (Batch.find_model c.solver c.pc) ~f:(fun (s, v) ->
          let sort = Types.string_of_type (Symbol.type_of s)
          and name = Symbol.to_string s
          and interp = Value.to_string v in
          (sort, name, interp))
    in
    let pc = (Expression.string_of_pc c.pc, Expression.to_smt c.pc) in
    let sink =
      match c.code with
      | Failure (sink, e) -> (sink, Option.value_map e ~default:"" ~f:Expr.str)
      | _ -> ("", "")
    in
    (sink, pc, testcase)
  in
  let serialize cs prefix =
    let cs' = List.map ~f cs in
    let prefix' = Filename.concat testsuite_path prefix in
    let sinks = List.map ~f:(fun (sink, _, _) -> sink) cs' in
    let queries = List.map ~f:(fun (_, pc, _) -> pc) cs' in
    let testsuite = List.map ~f:(fun (_, _, testcase) -> testcase) cs' in
    Report.serialise_sinks sinks prefix';
    Report.serialise_queries queries prefix';
    Report.serialise_testsuite testsuite prefix'
  in
  serialize final_configs "testcase";
  serialize error_configs "witness";
  Report.serialise_report
    (Filename.concat !Config.workspace "report.json")
    !Config.file (List.length configs)
    (List.length error_configs)
    0 !time_analysis !Batch.solver_time

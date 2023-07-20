open Core
open Func
open Expr
open Source
module Crash = Err.Make ()
module Invalid_arg = Err.Make ()

exception Crash = Crash.Error
exception Invalid_arg = Invalid_arg.Error

let ( let* ) o f = Result.bind o ~f
let return x = Result.Ok x

module Make (P : Eval_functor_intf.P) = struct
  module Store = P.Store
  module Object = P.Object
  module Heap = P.Heap
  module Reducer = P.Reducer

  module State = struct
    module ESet = Set.Make (Encoding.Expression)

    type pc = ESet.t
    type func = string
    type stack = Store.t Call_stack.t
    type state = Heap.t * Store.t * stack * func

    type outcome =
      | Cont of Stmt.t list
      | Error of Expr.t option
      | Final of Expr.t option
      | Failure of string * Expr.t option
      | Unknown of Expr.t option

    type config = {
      prog : Prog.t;
      code : outcome;
      state : state;
      pc : pc;
      solver : Batch.t;
      opt : Encoding.Optimizer.t;
    }

    let is_cont (o : outcome) : bool =
      match o with Cont _ -> true | _ -> false

    let is_fail (o : outcome) : bool =
      match o with Failure _ -> true | _ -> false

    let is_final (o : outcome) : bool =
      match o with Final _ -> true | _ -> false

    let update (c : config) code state pc : config = { c with code; state; pc }
  end

  (* Eval pass to remove variables from store *)
  let rec eval_expr (store : Store.t) (e : Expr.t) : Expr.t =
    match e with
    | Val v -> Val v
    | Var x ->
        Option.value_exn (Store.find store x)
          ~message:(sprintf "Cannot find var '%s'" x)
    | UnOpt (op, e') -> UnOpt (op, eval_expr store e')
    | BinOpt (op, e1, e2) -> BinOpt (op, eval_expr store e1, eval_expr store e2)
    | TriOpt (op, e1, e2, e3) ->
        TriOpt (op, eval_expr store e1, eval_expr store e2, eval_expr store e3)
    | NOpt (op, es) -> NOpt (op, List.map ~f:(eval_expr store) es)
    | Curry (f, es) -> Curry (f, List.map ~f:(eval_expr store) es)
    | Symbolic (t, x) -> Symbolic (t, eval_expr store x)

  let stmts b msg = match b with State.Cont stmts -> Ok stmts | _ -> Error msg
  let block b msg = match b.it with Stmt.Block b -> Ok b | _ -> Error msg

  let func v =
    match v with
    | Val (Val.Str x) -> Ok (x, [])
    | Curry (Val (Val.Str x), vs) -> Ok (x, vs)
    | _ -> Error "Sval is not a 'func' identifier"

  let rec unfold_ite ~(accum : Expr.t) (e : Expr.t) :
      (Expr.t Option.t * String.t) List.t =
    let open Operators in
    match e with
    | Val (Val.Loc x) | Val (Val.Symbol x) -> [ (Some accum, x) ]
    | TriOpt (ITE, c, Val (Val.Loc l), e) ->
        let accum' = BinOpt (Log_And, accum, UnOpt (Not, c)) in
        let tl = unfold_ite ~accum:accum' e in
        (Some (BinOpt (Log_And, accum, c)), l) :: tl
    | _ ->
        Printf.printf "rip with %s\n" (Expr.str e);
        assert false

  let loc (e : Expr.t) : ((Expr.t Option.t * String.t) List.t, string) Result.t
      =
    match e with
    | Val (Val.Loc l) -> Ok [ (None, l) ]
    | TriOpt (Operators.ITE, c, Val (Val.Loc l), v) ->
        Ok ((Some c, l) :: unfold_ite ~accum:(UnOpt (Operators.Not, c)) v)
    | _ -> Error ("Expr '" ^ Expr.str e ^ "' is not a loc expression")

  let step (c : State.config) : (State.config list, string) Result.t =
    let open Stmt in
    let open State in
    let { prog; code; state; pc; solver; opt } = c in
    let heap, store, stack, f = state in
    let* stmts = stmts code "step: Empty continuation!" in
    let s = List.hd_exn stmts in
    (*
  (if Stmt.is_basic_stmt s then
     let str_e (e : Expr.t) : string = Expr.str e in
     let debug =
       lazy
         (sprintf
            "====================================\nEvaluating >>>>> %s: %s (%s)"
            f (Stmt.str s)
            (Stmt.str ~print_expr:str_e s))
     in
     Logging.print_endline debug
     );
     *)
    match s.it with
    | Skip -> return [ update c (Cont (List.tl_exn stmts)) state pc ]
    | Merge -> return [ update c (Cont (List.tl_exn stmts)) state pc ]
    | Exception err ->
        fprintf stderr "%s: Exception: %s\n" (Source.string_of_region s.at) err;
        return [ update c (Error (Some (Expr.Val (Val.Str err)))) state pc ]
    | Print e ->
        let e' = Reducer.reduce_expr ~at:s.at store e in
        let s =
          match e' with
          | Val (Val.Loc l) ->
              let o = Heap.get heap l in
              Object.to_string (Option.value_exn o) Expr.str
          | _ -> Expr.str e'
        in
        (* Printf.printf "print:%s\npc:%s\nheap id:%d\n" s (Encoding.Expression.string_of_pc pc) (Heap.get_id heap); *)
        Format.printf "%s@." s;
        return [ update c (Cont (List.tl_exn stmts)) state pc ]
    | Fail e ->
        return
          [ update c (Error (Some (Reducer.reduce_expr ~at:s.at store e))) state pc ]
    | Abort e ->
        return
          [
            update c
              (Failure ("Abort", Some (Reducer.reduce_expr ~at:s.at store e)))
              state pc;
          ]
    | Stmt.Assign (x, e) ->
        let v = Reducer.reduce_expr ~at:s.at store e in
        return
          [
            update c
              (Cont (List.tl_exn stmts))
              (heap, Store.add_exn store x v, stack, f)
              pc;
          ]
    | Stmt.Assert e -> (
        match Reducer.reduce_expr ~at:s.at store e with
        | Val (Val.Bool b) ->
            if b then return [ update c (Cont (List.tl_exn stmts)) state pc ]
            else
              let e' = Some (Reducer.reduce_expr ~at:s.at store e) in
              return [ update c (Failure ("assert", e')) state pc ]
        | v ->
            let v' =
              Reducer.reduce_expr ~at:s.at store (Expr.UnOpt (Operators.Not, v))
            in
            let cont =
              let pc' = ESet.add pc (Translator.translate v') in
              if Batch.check solver (ESet.to_list pc') then
                [ update c (Failure ("assert", Some v)) state pc' ]
              else [ update c (Cont (List.tl_exn stmts)) state pc ]
            in
            Logging.print_endline
              (lazy
                ("assert (" ^ Expr.str v ^ ") = "
                ^ Bool.to_string (is_cont c.code)));
            return cont)
    | Stmt.Block blk ->
        return [ update c (Cont (blk @ List.tl_exn stmts)) state pc ]
    | Stmt.If (br, blk, _)
      when Expr.equal (Val (Val.Bool true)) (Reducer.reduce_expr ~at:s.at store br) ->
        let* b = block blk "Malformed if statement 'then' block!" in
        let cont = b @ ((Stmt.Merge @@ blk.at) :: List.tl_exn stmts) in
        return [ update c (Cont cont) state pc ]
    | Stmt.If (br, _, blk)
      when Expr.equal (Val (Val.Bool false)) (Reducer.reduce_expr ~at:s.at store br) ->
        let cont =
          let t = List.tl_exn stmts in
          match blk with
          | None -> t
          | Some s' -> s' :: (Stmt.Merge @@ s'.at) :: t
        in
        return [ update c (Cont cont) state pc ]
    | Stmt.If (br, blk1, blk2) ->
        let br' = eval_expr store br in
        let br_t = Reducer.reduce_expr ~at:s.at store br'
        and br_f =
          Reducer.reduce_expr ~at:s.at store (Expr.UnOpt (Operators.Not, br'))
        in
        Logging.print_endline
          (lazy
            (sprintf "%s: If (%s)"
               (Source.string_of_region s.at)
               (Expr.str br_t)));
        let br_t' = Translator.translate br_t
        and br_f' = Translator.translate br_f in
        let then_branch =
          let pc' = ESet.add pc br_t' in
          if not (Batch.check solver (ESet.to_list pc')) then []
          else
            let state' = (Heap.clone heap, store, stack, f) in
            let stmts' = blk1 :: (Stmt.Merge @@ blk1.at) :: List.tl_exn stmts in
            [ update c (Cont stmts') state' pc' ]
        in
        let else_branch =
          let pc' = ESet.add pc br_f' in
          if not (Batch.check solver (ESet.to_list pc')) then []
          else
            let state' = (Heap.clone heap, store, stack, f) in
            let stmts' =
              match blk2 with
              | None -> List.tl_exn stmts
              | Some s' -> s' :: (Stmt.Merge @@ s'.at) :: List.tl_exn stmts
            in
            [ update c (Cont stmts') state' pc' ]
        in
        return (else_branch @ then_branch)
    | Stmt.While (br, blk) ->
        let blk' =
          Stmt.Block (blk :: [ Stmt.While (br, blk) @@ s.at ]) @@ blk.at
        in
        let cont = (Stmt.If (br, blk', None) @@ s.at) :: List.tl_exn stmts in
        return [ update c (Cont cont) state pc ]
    | Stmt.Return e -> (
        let v = Reducer.reduce_expr ~at:s.at store e in
        let frame, stack' = Call_stack.pop stack in
        match frame with
        | Call_stack.Intermediate (stmts', store', x, f') ->
            let state' = (heap, Store.add_exn store' x v, stack', f') in
            return [ update c (Cont stmts') state' pc ]
        | Call_stack.Toplevel -> return [ update c (Final (Some v)) state pc ])
    | Stmt.AssignCall (x, e, es) ->
        let* f', vs = func (Reducer.reduce_expr ~at:s.at store e) in
        let vs' = vs @ List.map ~f:(Reducer.reduce_expr ~at:s.at store) es in
        let func = Prog.get_func prog f' in
        let stack' =
          Call_stack.push stack
            (Call_stack.Intermediate (List.tl_exn stmts, store, x, f))
        in
        let store' =
          Store.create (List.zip_exn (Prog.get_params prog f') vs')
        in
        return [ update c (Cont [ func.body ]) (heap, store', stack', f') pc ]
    | Stmt.AssignECall (x, y, es) ->
        Crash.error s.at "'AssignECall' not implemented!"
    | Stmt.AssignNewObj x ->
        let obj = Object.create () in
        let loc = Heap.insert heap obj in
        let state' =
          (heap, Store.add_exn store x (Val (Val.Loc loc)), stack, f)
        in
        return [ update c (Cont (List.tl_exn stmts)) state' pc ]
    | Stmt.AssignInObjCheck (x, e_field, e_loc) ->
        let* locs = loc (Reducer.reduce_expr ~at:s.at store e_loc) in
        let field = Reducer.reduce_expr ~at:s.at store e_field in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   let v = Heap.has_field heap' l field in
                   let state' = (heap', Store.add_exn store x v, stack, f) in
                   update c (Cont (List.tl_exn stmts)) state' pc :: accum
               | Some cond' ->
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check solver (ESet.to_list pc')) then accum
                   else
                     let v = Heap.has_field heap l field in
                     let state' =
                       (Heap.clone heap, Store.add_exn store x v, stack, f)
                     in
                     update c (Cont (List.tl_exn stmts)) state' pc' :: accum))
    | Stmt.AssignObjToList (x, e) ->
        let f h l pc' =
          let v =
            match Heap.get h l with
            | None -> Crash.error s.at ("'" ^ l ^ "' not found in heap")
            | Some obj ->
                NOpt
                  ( Operators.ListExpr,
                    List.map (Object.to_list obj) ~f:(fun (f, v) ->
                        NOpt (Operators.TupleExpr, [ f; v ])) )
          in
          update c
            (Cont (List.tl_exn stmts))
            (h, Store.add_exn store x v, stack, f)
            pc'
        in
        let* locs = loc (Reducer.reduce_expr ~at:s.at store e) in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   f heap' l pc :: accum
               | Some cond' ->
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check solver (ESet.to_list pc')) then accum
                   else f (Heap.clone heap) l pc' :: accum))
    | Stmt.AssignObjFields (x, e) ->
        let f h l pc' =
          let v =
            match Heap.get h l with
            | None -> Crash.error s.at ("'" ^ l ^ "' not found in heap")
            | Some obj -> NOpt (Operators.ListExpr, Object.get_fields obj)
          in
          update c
            (Cont (List.tl_exn stmts))
            (h, Store.add_exn store x v, stack, f)
            pc'
        in
        let* locs = loc (Reducer.reduce_expr ~at:s.at store e) in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   f heap' l pc :: accum
               | Some cond' ->
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check solver (ESet.to_list pc')) then accum
                   else f (Heap.clone heap) l pc' :: accum))
    | Stmt.FieldAssign (e_loc, e_field, e_v) ->
        let* locs = loc (Reducer.reduce_expr ~at:s.at store e_loc) in
        let reduced_field = Reducer.reduce_expr ~at:s.at store e_field
        and v = Reducer.reduce_expr ~at:s.at store e_v in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   let objects =
                     Heap.set_field heap' l reduced_field v solver
                       (ESet.to_list pc) store
                   in
                   List.map objects ~f:(fun (new_heap, new_pc) ->
                       let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                       update c
                         (Cont (List.tl_exn stmts))
                         (new_heap, store, stack, f)
                         pc')
               | Some cond' ->
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check solver (ESet.to_list pc')) then accum
                   else
                     let objects =
                       Heap.set_field heap l reduced_field v solver
                         (ESet.to_list pc) store
                     in
                     List.map objects ~f:(fun (new_heap, new_pc) ->
                         let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                         update c
                           (Cont (List.tl_exn stmts))
                           (new_heap, store, stack, f)
                           pc')))
    | Stmt.FieldDelete (e_loc, e_field) ->
        let* locs = loc (Reducer.reduce_expr ~at:s.at store e_loc) in
        let reduced_field = Reducer.reduce_expr ~at:s.at store e_field in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   let objects =
                     Heap.delete_field heap' l reduced_field solver
                       (ESet.to_list pc) store
                   in
                   List.map objects ~f:(fun (new_heap, new_pc) ->
                       let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                       update c
                         (Cont (List.tl_exn stmts))
                         (new_heap, store, stack, f)
                         pc')
               | Some cond' ->
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check solver (ESet.to_list pc')) then accum
                   else
                     let objects =
                       Heap.delete_field heap l reduced_field solver
                         (ESet.to_list pc) store
                     in
                     List.map objects ~f:(fun (new_heap, new_pc) ->
                         let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                         update c
                           (Cont (List.tl_exn stmts))
                           (new_heap, store, stack, f)
                           pc')))
    | Stmt.FieldLookup (x, e_loc, e_field) ->
        let* locs = loc (Reducer.reduce_expr ~at:s.at store e_loc) in
        let reduced_field = Reducer.reduce_expr ~at:s.at store e_field in
        return
          (List.fold locs ~init:[] ~f:(fun accum (cond, l) ->
               match cond with
               | None ->
                   let heap' =
                     if List.length locs > 1 then Heap.clone heap else heap
                   in
                   let objects =
                     Heap.get_field heap' l reduced_field solver
                       (ESet.to_list pc) store
                   in
                   List.map objects ~f:(fun (new_heap, new_pc, v) ->
                       let v' =
                         Option.value v ~default:(Val (Val.Symbol "undefined"))
                       in
                       let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                       let state' =
                         (new_heap, Store.add_exn store x v', stack, f)
                       in
                       update c (Cont (List.tl_exn stmts)) state' pc')
               | Some cond' ->
                   let pc' = ESet.add pc (Translator.translate cond') in
                   if not (Batch.check solver (ESet.to_list pc')) then accum
                   else
                     let objects =
                       Heap.get_field heap l reduced_field solver
                         (ESet.to_list pc) store
                     in
                     List.map objects ~f:(fun (new_heap, new_pc, v) ->
                         let v' =
                           Option.value v
                             ~default:(Val (Val.Symbol "undefined"))
                         in
                         let pc' = List.fold new_pc ~init:pc ~f:ESet.add in
                         update c
                           (Cont (List.tl_exn stmts))
                           (new_heap, Store.add_exn store x v', stack, f)
                           pc')))
    | Stmt.SymStmt (SymStmt.Assume e) -> (
        match Reducer.reduce_expr ~at:s.at store e with
        | Val (Val.Bool b) ->
            return
              (if b then [ update c (Cont (List.tl_exn stmts)) state pc ]
              else [])
        | e' ->
            let pc' = ESet.add pc (Translator.translate e') in
            let cont =
              if not (Batch.check solver (ESet.to_list pc')) then []
              else [ update c (Cont (List.tl_exn stmts)) state pc' ]
            in
            Logging.print_endline
              (lazy
                ("assume (" ^ Expr.str e' ^ ") = "
                ^ Bool.to_string (List.length cont > 0)));
            return cont)
    | Stmt.SymStmt (SymStmt.Evaluate (x, e)) ->
        let e' = Translator.translate (Reducer.reduce_expr ~at:s.at store e) in
        let _sym_e = List.hd (Encoding.Expression.get_symbols [ e' ]) in
        assert (Batch.check solver (ESet.to_list pc));
        assert false
    | Stmt.SymStmt (SymStmt.Maximize (x, e)) ->
        let e' = Translator.translate (Reducer.reduce_expr ~at:s.at store e) in
        let v =
          Option.map ~f:Translator.expr_of_value
            (Encoding.Optimizer.maximize opt e' (ESet.to_list pc))
        in
        let store' =
          Store.add_exn store x (Option.value ~default:(Val Val.Null) v)
        in
        return
          [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
    | Stmt.SymStmt (SymStmt.Minimize (x, e)) ->
        let e' = Translator.translate (Reducer.reduce_expr ~at:s.at store e) in
        let v =
          Option.map ~f:Translator.expr_of_value
            (Encoding.Optimizer.minimize opt e' (ESet.to_list pc))
        in
        let store' =
          Store.add_exn store x (Option.value ~default:(Val Val.Null) v)
        in
        return
          [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
    | Stmt.SymStmt (SymStmt.Is_symbolic (x, e)) ->
        let e' = Reducer.reduce_expr ~at:s.at store e in
        let store' =
          Store.add_exn store x (Val (Val.Bool (Expr.is_symbolic e')))
        in
        return
          [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
    | Stmt.SymStmt (SymStmt.Is_sat (x, e)) ->
        let e' = Reducer.reduce_expr ~at:s.at store e in
        let pc' = ESet.add pc (Translator.translate e') in
        let sat = Batch.check c.solver (ESet.to_list pc') in
        let store' = Store.add_exn store x (Val (Val.Bool sat)) in
        return
          [ update c (Cont (List.tl_exn stmts)) (heap, store', stack, f) pc ]
    | Stmt.SymStmt (SymStmt.Is_number (x, e)) ->
        let e' = Reducer.reduce_expr ~at:s.at store e in
        let is_num =
          match Sval_typing.type_of e' with
          | Some Type.IntType | Some Type.FltType -> true
          | _ -> false
        in
        let store' = Store.add_exn store x (Val (Val.Bool is_num)) in
        return
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
    open State

    let eval c : State.config list =
      let w = L.create () in
      L.push c w;
      let out = ref [] in
      while not (L.is_empty w) do
        let c = L.pop w in
        match c.code with
        | Cont [] ->
            let _, _, _, f = c.state in
            Crash.error Source.no_region
              (sprintf "%s: eval: Empty continuation!" f)
        | Cont _ -> (
            match step c with
            | Ok cs -> List.iter ~f:(fun c -> L.push c w) cs
            | Error msg -> Crash.error Source.no_region msg)
        | Error v | Final v | Unknown v -> out := c :: !out
        | Failure (f, e) ->
            let _, _, _, func = c.state in
            let e' = Option.value_map e ~default:"" ~f:Expr.str in
            Logging.print_endline
              (lazy (sprintf "Failure: %s: %s: %s" func f e'));
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
  let invoke (prog : Prog.t) (func : Func.t)
      (eval : State.config -> State.config list) : State.config list =
    let heap = Heap.create ()
    and store = Store.create []
    and stack = Call_stack.push Call_stack.empty Call_stack.Toplevel in
    let solver =
      let s = Batch.create () in
      if !Config.axioms then Batch.add s Encoding.Axioms.axioms;
      s
    in
    let initial_config =
      State.
        {
          prog;
          code = Cont [ func.body ];
          state = (heap, store, stack, func.name);
          pc = ESet.empty;
          solver;
          opt = Encoding.Optimizer.create ();
        }
    in
    eval initial_config

  let analyse (prog : Prog.t) (f : State.func) (policy : string) :
      State.config list =
    let f = Prog.get_func prog f in
    let eval =
      match policy with
      | "breadth" -> BFS.eval
      | "depth" -> DFS.eval
      | "random" -> RND.eval
      | _ ->
          Crash.error f.body.at
            ("Invalid search policy '" ^ !Config.policy ^ "'")
    in
    invoke prog f eval

  let main (prog : Prog.t) (f : State.func) : unit =
    let open State in
    let time_analysis = ref 0.0 in
    let configs =
      Time_utils.time_call time_analysis (fun () ->
          analyse prog f !Config.policy)
    in
    let testsuite_path = Filename.concat !Config.workspace "test-suite" in
    Io.safe_mkdir testsuite_path;
    let final_configs = List.filter ~f:(fun c -> State.is_final c.code) configs
    and error_configs =
      List.filter ~f:(fun c -> State.is_fail c.code) configs
    in
    let f c =
      let pc' = State.ESet.to_list c.pc in
      assert (Batch.check c.solver pc');
      let model = Batch.model c.solver in
      let open Encoding in
      let testcase =
        Option.value_map model ~default:[] ~f:(fun m ->
            List.map (Model.get_bindings m) ~f:(fun (s, v) ->
                let sort = Types.string_of_type (Symbol.type_of s)
                and name = Symbol.to_string s
                and interp = Value.to_string v in
                (sort, name, interp)))
      in
      let pc = (Expression.string_of_pc pc', Expression.to_smt pc') in
      let sink =
        match c.code with
        | Failure (sink, e) ->
            (sink, Option.value_map e ~default:"" ~f:Expr.str)
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
      0 !time_analysis 0.0
end

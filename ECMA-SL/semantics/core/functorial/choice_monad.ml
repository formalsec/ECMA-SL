open EslBase
module Value = Symbolic_value.M
module Memory = Symbolic_memory
module Optimizer = Smtml.Optimizer.Z3
module E = Smtml.Expr


module PC = struct
  include Set.Make (struct
    include Smtml.Expr

    let compare = compare
  end)

  let to_list (s : t) = elements s [@@inline]
end

module Thread = struct
  type t =
    { solver : Solver.t
    ; pc : PC.t
    ; mem : Memory.t
    ; optimizer : Optimizer.t
    }

  let create () =
    { solver = Solver.create ()
    ; pc = PC.empty
    ; mem = Memory.create ()
    ; optimizer = Optimizer.create ()
    }

  let solver t = t.solver
  let pc t = t.pc
  let mem t = t.mem
  let optimizer t = t.optimizer

  let add_pc t (v : Smtml.Expr.t) =
    match Smtml.Expr.view v with
    | Val True -> t
    | _ -> { t with pc = PC.add v t.pc }

  let clone { solver; optimizer; pc; mem } =
    let mem = Memory.clone mem in
    { solver; optimizer; pc; mem }
end

module List = struct
  type thread = Thread.t
  type 'a t = thread -> ('a * thread) list

  let return (v : 'a) : 'a t = fun t -> [ (v, t) ]
  let run (v : 'a t) (thread : thread) = v thread

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let lst = run v t in
    match lst with
    | [] -> []
    | [ (r, t') ] -> run (f r) t'
    | _ -> List.concat_map (fun (r, t') -> run (f r) t') lst

  let ( let* ) v f = bind v f
  let map (v : 'a t) (f : 'a -> 'b) : 'b t = bind v (fun a -> return (f a))
  let ( let+ ) v f = map v f

  let with_thread (f : thread -> 'a) : 'a t =
   fun t ->
    let v = f t in
    [ (v, t) ]

  let check (cond : Value.value) : bool t =
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match E.view cond with
      | Val True -> [ (true, t) ]
      | Val False -> [ (false, t) ]
      | _ -> (
        let pc = PC.(add cond pc |> elements) in
        match Solver.check solver pc with
        | `Sat -> [ (true, t) ]
        | `Unsat -> [ (false, t) ]
        | `Unknown ->
          Format.eprintf "Unknown pc: %a@." Smtml.Expr.pp_list pc;
          [] )

  let check_add_true (cond : Value.value) : bool t =
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match E.view cond with
      | Val True -> [ (true, t) ]
      | Val False -> [ (false, t) ]
      | _ -> (
        let pc = PC.(add cond pc |> elements) in
        match Solver.check solver pc with
        | `Sat -> [ (true, Thread.add_pc t cond) ]
        | `Unsat -> [ (false, t) ]
        | `Unknown ->
          Format.eprintf "Unknown pc: %a@." Smtml.Expr.pp_list pc;
          [] )

  let branch (v : Value.value) : bool t =
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match E.view v with
      | Val True -> [ (true, t) ]
      | Val False -> [ (false, t) ]
      | _ -> (
        let with_v = PC.add v pc in
        let with_no = PC.add (Value.Bool.not_ v) pc in
        let sat_true =
          if PC.equal with_v pc then true
          else `Sat = Solver.check solver (PC.elements with_v)
        in
        let sat_false =
          if PC.equal with_no pc then true
          else `Sat = Solver.check solver (PC.to_list with_no)
        in
        match (sat_true, sat_false) with
        | (false, false) -> []
        | (true, false) | (false, true) -> [ (sat_true, t) ]
        | (true, true) ->
          let t0 = Thread.clone t in
          let t1 = Thread.clone t in
          [ (true, { t0 with pc = with_v }); (false, { t1 with pc = with_no }) ]
        )

  let select_val (v : Value.value) thread =
    match E.view v with
    | Val v -> [ (v, thread) ]
    | _ -> Log.fail "Unable to select value from %a" Value.pp v
end

module P : Choice_monad_intf.Complete with module V := Value = List

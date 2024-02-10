module Value = Symbolic_value.M
module Memory = Symbolic_memory
module Translator = Value_translator
module Optimizer = Encoding.Optimizer.Z3

module Thread = struct
  type t =
    { solver : Solver.t
    ; pc : Encoding.Expr.t list
    ; mem : Memory.t
    ; optimizer : Optimizer.t
    }

  let create () =
    { solver = Solver.create ()
    ; pc = []
    ; mem = Memory.create ()
    ; optimizer = Optimizer.create ()
    }

  let solver t = t.solver
  let pc t = t.pc
  let mem t = t.mem
  let optimizer t = t.optimizer
  let add_pc t v = { t with pc = v :: t.pc }
  let clone_mem t = { t with mem = Memory.clone t.mem }
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

  let map (v : 'a t) (f : 'a -> 'b) : 'b t = bind v (fun a -> return (f a))

  let check (v : Value.value) : bool t =
    let open Value in
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match v with
      | Val (Val.Bool b) -> [ (b, t) ]
      | _ ->
        let cond = Translator.translate v in
        [ (Solver.check solver (cond :: pc), t) ]

  let check_add_true (v : Value.value) : bool t =
    let open Value in
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match v with
      | Val (Val.Bool b) -> [ (b, t) ]
      | _ ->
        let cond' = Translator.translate v in
        if Solver.check solver (cond' :: pc) then
          [ (true, Thread.add_pc t cond') ]
        else [ (false, t) ]

  let branch (v : Value.value) : bool t =
    let open Value in
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match v with
      | Val (Val.Bool b) -> [ (b, t) ]
      | _ -> (
        let cond = Translator.translate v in
        let no = Translator.translate @@ Value.Bool.not_ v in
        let sat_true = Solver.check solver (cond :: pc) in
        let sat_false = Solver.check solver (no :: pc) in
        match (sat_true, sat_false) with
        | (false, false) -> []
        | (true, false) -> [ (true, Thread.add_pc t cond) ]
        | (false, true) -> [ (false, Thread.add_pc t no) ]
        | (true, true) ->
          let t0 = Thread.clone_mem t in
          let t1 = Thread.clone_mem t in
          [ (true, Thread.add_pc t0 cond); (false, Thread.add_pc t1 no) ] )

  let error (v : string) : 'a t =
    Format.printf "%s@." v;
    fun _ -> []
end

module P : Choice_monad_intf.Complete with module V := Value = List

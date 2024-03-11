open EslCore
open EslSyntax
module Value = Symbolic_value.M
module Memory = Symbolic_memory
module Translator = Value_translator
module Optimizer = Encoding.Optimizer.Z3

module PC = struct
  include Set.Make (struct
    include Encoding.Expr

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

  let add_pc t (v : Encoding.Expr.t) =
    match v.node.e with Val True -> t | _ -> { t with pc = PC.add v t.pc }

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

  let check (v : Value.value) : bool t =
    let open Value in
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match v with
      | Val (Val.Bool b) -> [ (b, t) ]
      | _ ->
        let cond = Translator.translate v in
        [ (Solver.check solver PC.(add cond pc |> elements), t) ]

  let check_add_true (v : Value.value) : bool t =
    let open Value in
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match v with
      | Val (Val.Bool b) -> [ (b, t) ]
      | _ ->
        let cond' = Translator.translate v in
        if Solver.check solver PC.(add cond' pc |> elements) then
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
        let with_v = PC.add (Translator.translate v) pc in
        let with_no = PC.add (Translator.translate @@ Value.Bool.not_ v) pc in
        let sat_true =
          if PC.equal with_v pc then true
          else Solver.check solver (PC.elements with_v)
        in
        let sat_false =
          if PC.equal with_no pc then true
          else Solver.check solver (PC.to_list with_no)
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
    match v with
    | Val v -> [ (v, thread) ]
    | _ -> Log.err "Unable to select value from %a" Value.pp v
end

module P : Choice_monad_intf.Complete with module V := Value = List

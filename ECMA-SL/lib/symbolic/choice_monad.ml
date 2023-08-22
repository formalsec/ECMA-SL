module Value = Sym_value.M
module Heap = Sym_heap2.Heap

module Thread = struct
  type t =
    { solver : Batch.t
    ; pc : Encoding.Expression.t list
    ; mem : Sym_heap2.Heap.t
    ; optimizer : Encoding.Optimizer.t
    }

  let create () =
    { solver = Batch.create ()
    ; pc = []
    ; mem = Heap.create ()
    ; optimizer = Encoding.Optimizer.create ()
    }

  let solver t = t.solver
  let pc t = t.pc
  let mem t = t.mem
  let optimizer t = t.optimizer
  let add_pc t v = { t with pc = v :: t.pc }
  let clone_mem t = { t with mem = Heap.clone t.mem }
end

module List = struct
  type thread = Thread.t
  type 'a t = thread -> ('a * thread) list

  let return (v : 'a) : 'a t = fun t -> [ (v, t) ]

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let lst = v t in
    match lst with
    | [] -> []
    | [ (r, t') ] -> (f r) t'
    | _ -> List.concat_map (fun (r, t') -> (f r) t') lst

  let select (v : Value.value) : bool t =
    let open Value in
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match v with
      | Val (Val.Bool b) -> [ (b, t) ]
      | _ ->
        let cond = Value_translator.translate v in
        [ (Batch.check solver (cond :: pc), t) ]

  let branch (v : Value.value) : bool t =
    let open Value in
    fun t ->
      let solver = Thread.solver t in
      let pc = Thread.pc t in
      match v with
      | Val (Val.Bool b) -> [ (b, t) ]
      | _ -> (
        let cond = Value_translator.translate v in
        let no = Value_translator.translate @@ Value.Bool.not_ v in
        let sat_true = Batch.check solver (cond :: pc) in
        let sat_false = Batch.check solver (no :: pc) in
        match (sat_true, sat_false) with
        | false, false -> []
        | true, false -> [ (true, Thread.add_pc t cond) ]
        | false, true -> [ (false, Thread.add_pc t no) ]
        | true, true ->
          let t0 = Thread.clone_mem t in
          let t1 = Thread.clone_mem t in
          [ (true, Thread.add_pc t0 cond); (false, Thread.add_pc t1 no) ] )

  let run (v : 'a t) (thread : thread) = v thread

  let error (v : string) : 'a t =
    Format.printf "%s" v;
    fun _ -> []
end

module P : Choice_monad_intf.Complete with module V := Value = List

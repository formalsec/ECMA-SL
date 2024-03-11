open EslCore
module Value = Symbolic_value.M
module Store = Value.Store
module Object = Symbolic_object.M
module Memory = Symbolic_memory
module Env = Link_env.Make (Memory)
module Thread = Choice_monad.Thread
module Translator = Value_translator
module PC = Choice_monad.PC

module P = struct
  type value = Value.value
  type store = Store.t
  type memory = Memory.t
  type object_ = Object.t

  module Value = struct
    include Value
  end

  module Choice = Choice_monad.List
  module Extern_func = Extern_func.Make (Value) (Choice)

  let ( let*/ ) o f = match o with Error e -> failwith e | Ok o -> f o

  type extern_func = Extern_func.extern_func
  type env = extern_func Env.t

  module Store = struct
    type bind = string
    type t = store

    let create lst = Store.create lst [@@inline]
    let mem store = Store.mem store [@@inline]
    let add_exn store key data = Store.add_exn store key data [@@inline]
    let find store v = Store.find store v [@@inline]
  end

  module Object = struct
    type t = object_
    type nonrec value = value

    let create () = Object.create () [@@inline]
    let to_string o = Object.to_string o [@@inline]
    let set o ~key ~data = Object.set o ~key ~data [@@inline]

    let get o key =
      let vals = Object.get o key in
      let return thread (v, pc) =
        let pc_thread = Thread.pc thread in
        let solver = Thread.solver thread in
        match pc with
        | [] -> Some (Some v, thread)
        | _ ->
          let pc' = List.map Translator.translate pc in
          let pc' = PC.(union pc_thread (of_list pc')) in
          if PC.equal pc' pc_thread then Some (Some v, thread)
          else if not (Solver.check solver (PC.elements pc')) then None
          else Some (Some v, { thread with pc = pc' })
      in
      match vals with
      | [] -> fun thread -> [ (None, thread) ]
      | [ (v, pc) ] ->
        fun thread ->
          Option.fold ~none:[] ~some:(fun r -> [ r ]) (return thread (v, pc))
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          List.filter_map (return thread) vals

    let delete o v = Object.delete o v [@@inline]
    let to_list o = Object.to_list o [@@inline]
    let has_field o v = Object.has_field o v [@@inline]
    let get_fields o = Object.get_fields o [@@inline]
  end

  module Memory = struct
    type t = memory
    type nonrec object_ = object_
    type nonrec value = value

    let create () = Memory.create () [@@inline]
    let clone m = Memory.clone m [@@inline]
    let insert m o = Memory.insert m o [@@inline]
    let remove m loc = Memory.remove m loc [@@inline]
    let set m loc o = Memory.set m loc o [@@inline]
    let get m loc = Memory.get m loc [@@inline]
    let has_field m x v = Memory.has_field m x v [@@inline]

    let get_field h loc v =
      let field_vals = Memory.get_field h loc v in
      let return thread (v, pc) =
        let pc_thread = Thread.pc thread in
        let solver = Thread.solver thread in
        match pc with
        | [] -> Some (Some v, thread)
        | _ ->
          let pc' = List.map Translator.translate pc in
          let pc' = PC.(union pc_thread (of_list pc')) in
          if PC.equal pc' pc_thread then Some (Some v, thread)
          else if not (Solver.check solver (PC.elements pc')) then None
          else Some (Some v, { thread with pc = pc' })
      in
      match field_vals with
      | [] -> fun thread -> [ (None, thread) ]
      | [ (v, pc) ] -> (
        fun thread ->
          match return thread (v, pc) with None -> [] | Some a -> [ a ] )
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          List.filter_map (return thread) field_vals

    let set_field m loc ~field ~data = Memory.set_field m loc ~field ~data
    [@@inline]

    let delete_field m loc v = Memory.delete_field m loc v [@@inline]
    let to_string h = Format.asprintf "%a" Memory.pp h [@@inline]

    let loc v =
      let*/ locs = Memory.loc v in
      let return thread (cond, v) =
        let pc = Thread.pc thread in
        let solver = Thread.solver thread in
        match cond with
        | None -> Some (v, thread)
        | Some c ->
          let c' = Translator.translate c in
          let pc = PC.add c' pc in
          if PC.equal pc (Thread.pc thread) then Some (v, thread)
          else if not (Solver.check solver (PC.elements pc)) then None
          else Some (v, { thread with pc })
      in
      match locs with
      | [] ->
        fun _thread ->
          Log.log ~header:false "   symbolic : no loc";
          []
      | [ (c, v) ] -> (
        fun thread ->
          match return thread (c, v) with None -> [] | Some a -> [ a ] )
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          List.filter_map (return thread) locs

    let pp fmt v = Memory.pp fmt v [@@inline]
    let pp_val m v = Memory.pp_val m v [@@inline]
  end

  module Env = struct
    type t = env
    type nonrec memory = memory

    let clone env = Env.clone env [@@inline]

    let get_memory _env thread =
      (* Env.get_memory env *)
      [ (Thread.mem thread, thread) ]
    [@@inline]

    let get_func env func_id = Env.get_func env func_id [@@inline]
    let get_extern_func env func_id = Env.get_extern_func env func_id [@@inline]
    let add_memory env mem = Env.add_memory env mem [@@inline]
    let add_func env func_id func = Env.add_func env func_id func [@@inline]

    module Build = struct
      let empty () = Env.Build.empty () [@@inline]
      let add_memory m env = Env.Build.add_memory m env [@@inline]
      let add_functions funcs env = Env.Build.add_functions funcs env [@@inline]

      let add_extern_functions funcs env =
        Env.Build.add_extern_functions funcs env
      [@@inline]
    end
  end

  module Reducer = struct
    let reduce v = Value_reducer.reduce v [@@inline]
  end
end

module P' : Interpreter_functor_intf.P = P

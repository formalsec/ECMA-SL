open EslBase
module Value = Symbolic_value.M
module Store = Value.Store
module Object = Symbolic_object.M
module Memory = Symbolic_memory
module Env = Link_env.Make (Memory)
module Thread = Choice_monad.Thread

module P = struct
  type value = Value.value
  type store = Store.t
  type memory = Memory.t
  type object_ = Object.t

  module Value = struct
    include Value
  end

  module Choice = Choice_monad.Seq
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
        | _ -> (
          let pc' = Smtml.Expr.Set.(union pc_thread (of_list pc)) in
          if Smtml.Expr.Set.equal pc' pc_thread then Some (Some v, thread)
          else
            match Solver.check_set solver pc' with
            | `Sat -> Some (Some v, { thread with pc = pc' })
            | `Unsat | `Unknown -> None )
      in
      match vals with
      | [] -> Choice.return None
      | [ (v, pc) ] ->
        fun thread ->
          Option.fold ~none:Seq.empty ~some:Seq.return (return thread (v, pc))
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          List.to_seq @@ List.filter_map (return thread) vals

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
        | _ -> (
          let pc' = Smtml.Expr.Set.(union pc_thread (of_list pc)) in
          if Smtml.Expr.Set.equal pc' pc_thread then Some (Some v, thread)
          else
            match Solver.check_set solver pc' with
            | `Sat -> Some (Some v, { thread with pc = pc' })
            | `Unsat | `Unknown -> None )
      in
      match field_vals with
      | [] -> Choice.return None
      | [ (v, pc) ] -> (
        fun thread ->
          match return thread (v, pc) with
          | None -> Seq.empty
          | Some a -> Seq.return a )
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          List.to_seq @@ List.filter_map (return thread) field_vals

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
        | Some c -> (
          let pc = Smtml.Expr.Set.add c pc in
          if Smtml.Expr.Set.equal pc (Thread.pc thread) then Some (v, thread)
          else
            match Solver.check_set solver pc with
            | `Sat -> Some (v, { thread with pc })
            | `Unsat | `Unknown -> None )
      in
      match locs with
      | [] ->
        fun _thread ->
          Log.stdout "   symbolic : no loc@.";
          Seq.empty
      | [ (c, v) ] -> (
        fun thread ->
          match return thread (c, v) with
          | None -> Seq.empty
          | Some a -> Seq.return a )
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          List.to_seq @@ List.filter_map (return thread) locs

    let pp ppf v = Memory.pp ppf v [@@inline]
    let pp_val m v = Memory.pp_val m v [@@inline]
  end

  module Env = struct
    type t = env
    type nonrec memory = memory

    let clone env = Env.clone env [@@inline]
    let get_memory _env = Choice.with_thread Thread.mem [@@inline]
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
end

module P' : Interpreter_functor_intf.P = P

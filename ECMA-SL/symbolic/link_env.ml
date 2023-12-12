module type S = sig
  type 'a t
  type memory

  val clone : 'a t -> 'a t
  val get_memory : 'a t -> memory
  val get_func : 'a t -> string -> (Func.t, string) Result.t
  val get_extern_func : 'a t -> string -> ('a, string) Result.t
  val add_memory : 'a t -> memory -> 'a t
end

module SMap = Map.Make (String)

module Make (Memory : Sym_heap_intf.S) = struct
  type memory = Memory.t

  type 'a t =
    { memory : Memory.t
    ; functions : Prog.t
    ; extern_funcs : 'a SMap.t
    }

  let clone (env : 'a t) = { env with memory = Memory.clone env.memory }
  let get_memory (env : 'a t) = env.memory
  let get_func (env : 'a t) id = Prog.get_func env.functions id

  let get_extern_func (env : 'a t) id =
    match SMap.find_opt id env.extern_funcs with
    | Some f -> Ok f
    | None -> Error (Format.sprintf "unable to find external function '%s'" id)

  let add_memory (env : 'a t) memory = { env with memory }

  module Build = struct
    let empty () =
      { memory = Memory.create ()
      ; functions = Prog.empty ()
      ; extern_funcs = SMap.empty
      }

    let add_memory (env : 'a t) memory = { env with memory }
    let add_functions (env : 'a t) functions = { env with functions }

    let add_extern_functions (env : 'a t) extern_funcs =
      let extern_funcs' =
        SMap.fold
          (fun key data accum -> SMap.add key data accum)
          extern_funcs env.extern_funcs
      in
      { env with extern_funcs = extern_funcs' }
  end
end

open EslSyntax

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

module Make (Memory : Memory_intf.S) = struct
  type memory = Memory.t

  type 'a t =
    { memory : Memory.t
    ; functions : Prog.t
    ; extern_funcs : 'a SMap.t
    }

  let clone (env : 'a t) = { env with memory = Memory.clone env.memory }
  let get_memory (env : 'a t) = env.memory
  let get_func (env : 'a t) id = Prog.func env.functions id

  let get_extern_func (env : 'a t) id =
    match SMap.find id env.extern_funcs with
    | exception Not_found ->
      Error (Format.sprintf "unable to find external function '%s'" id)
    | f -> Ok f

  let add_memory (env : 'a t) memory = { env with memory }
  let add_func (env : 'a t) fid f = Prog.add_func env.functions fid f

  module Build = struct
    let empty () =
      { memory = Memory.create ()
      ; functions = Prog.default ()
      ; extern_funcs = SMap.empty
      }

    let add_memory memory (env : 'a t) = { env with memory }
    let add_functions functions (env : 'a t) = { env with functions }

    let add_extern_functions extern_funcs (env : 'a t) =
      let extern_funcs' =
        SMap.fold
          (fun key data accum -> SMap.add key data accum)
          extern_funcs env.extern_funcs
      in
      { env with extern_funcs = extern_funcs' }
  end
end

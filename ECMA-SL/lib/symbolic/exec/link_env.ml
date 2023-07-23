open Core

module type S = sig
  type t
  type memory

  val clone : t -> t
  val get_memory : t -> memory
  val get_func : t -> string -> (Func.t, string) Result.t
  val get_extern_func : t -> string -> (Extern_func.extern_func, string) Result.t
  val add_memory : t -> memory -> t
end

module SMap = Map.Make (String)

module Make (Memory : S_heap.S) = struct
  type memory = Memory.t

  type t = {
    memory : Memory.t;
    functions : Prog.t;
    extern_funcs : Extern_func.extern_func SMap.t;
  }

  let clone (env : t) = { env with memory = Memory.clone env.memory }
  let get_memory (env : t) = env.memory
  let get_func (env : t) id = Prog.get_func env.functions id

  let get_extern_func (env : t) id =
    Result.of_option
      (SMap.find env.extern_funcs id)
      ~error:(sprintf "unable to find external function '%s'" id)

  let add_memory (env : t) memory = { env with memory }

  module Build = struct
    let empty () =
      {
        memory = Memory.create ();
        functions = Prog.empty ();
        extern_funcs = SMap.empty;
      }

    let add_memory (env : t) memory = { env with memory }
    let add_functions (env : t) functions = { env with functions }
    let add_extern_functions (env : t) extern_funcs = { env with extern_funcs }
  end
end

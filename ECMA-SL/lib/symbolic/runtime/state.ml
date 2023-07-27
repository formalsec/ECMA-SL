module Value = Sym_value.M
module Store = Sym_value.M.Store
module Object = S_heap.Object
module Heap = S_heap.Heap
module Env = Link_env.Make (Heap)

module P = struct
  type value = Value.value
  type store = Store.t
  type memory = Heap.t
  type object_ = Object.t
  type env = Env.t

  module Value = struct
    include Value
  end

  module Choice = struct
    let assertion _solver _pc c =
      match c with Value.Val (Val.Bool b) -> b | _v -> assert false

    let assumption c =
      match c with Value.Val (Val.Bool b) -> Some b | _ -> None

    let branch _solver _pc c =
      match c with
      | Value.Val (Val.Bool b) -> ((b, None), (not b, None))
      | _ -> assert false
  end

  module Store = struct
    type bind = string
    type t = store

    let create = Store.create
    let mem = Store.mem
    let add_exn = Store.add_exn
    let find = Store.find
  end

  module Object = struct
    type t = object_
    type nonrec value = value
    type encoded_pct = Encoding.Expression.t

    let create = Object.create
    let clone = Object.clone
    let to_string = Object.to_string
    let set = Object.set
    let get = Object.get
    let delete = Object.delete
    let to_list = Object.to_list
    let has_field = Object.has_field
    let get_fields = Object.get_fields
  end

  module Heap = struct
    type t = memory
    type obj = object_
    type nonrec value = value
    type encoded_pct = Encoding.Expression.t

    let create = Heap.create
    let clone = Heap.clone
    let insert = Heap.insert
    let remove = Heap.remove
    let set = Heap.set
    let get = Heap.get
    let has_field = Heap.has_field
    let get_field = Heap.get_field
    let set_field = Heap.set_field
    let delete_field = Heap.delete_field
  end

  module Env = struct
    type t = env
    type nonrec memory = memory

    let clone = Env.clone
    let get_memory = Env.get_memory
    let get_func = Env.get_func
    let get_extern_func = Env.get_extern_func
    let add_memory = Env.add_memory

    module Build = struct
      let empty = Env.Build.empty
      let add_memory = Env.Build.add_memory
      let add_functions = Env.Build.add_functions
      let add_extern_functions = Env.Build.add_extern_functions
    end
  end

  module Reducer = struct
    let reduce = Reducer.reduce
  end

  module Translator = struct
    let translate = Translator.translate
    let expr_of_value = Translator.expr_of_value
  end
end

module P' : Eval_functor_intf.P = P

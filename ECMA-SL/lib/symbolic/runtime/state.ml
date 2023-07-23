module P = struct
  module S_heap = S_heap.Make (S_object.M)

  type value = Expr.t
  type store = S_store.t
  type memory = S_heap.t
  type object_ = S_object.M.t

  module Store = struct
    type bind = String.t
    type t = store

    let create = S_store.create
    let mem = S_store.mem
    let add_exn = S_store.add_exn
    let find = S_store.find
    let to_string = S_store.to_string
  end

  module Object = S_object.M
  module Heap = S_heap
  module Env = Link_env.Make (Heap)

  type env = Env.t

  module Reducer = Reducer
end

module P' : Eval_functor_intf.P = P

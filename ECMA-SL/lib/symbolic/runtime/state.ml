module P = struct
  module Store = struct
    include S_store
  end

  module Object = struct
    include S_object
  end

  module Heap = S_heap.MakeHeap(Object)

  module Reducer = struct
    include Reducer
  end
end

module P' : Eval_functor_intf.P = P

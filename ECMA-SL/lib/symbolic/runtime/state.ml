module P = struct
  module Store = struct
    include S_store
  end

  module Object = struct
    include S_object.M
  end

  module Heap = S_heap.Make(Object)

  module Reducer = struct
    include Reducer
  end
end

module P' : Eval_functor_intf.P = P

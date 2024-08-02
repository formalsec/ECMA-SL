open EslSyntax

type obj = Value.t Object.t
type store = Value.t Store.t
type heap = Value.t Heap.t
type stack = Value.t Store.t Call_stack.t

module IEntry = struct
  type t =
    { main : string
    ; heap : heap option
    }

  let default () : t = { main = "main"; heap = None }
end

module IResult = struct
  type t =
    { retval : Value.t
    ; heap : heap
    ; metrics : Yojson.Basic.t
    }
end

module IConst = struct
  let global_loc : Loc.t = 0
end

module IConfig = struct
  let print_depth : int option ref = ref None
  let resolve_exitval : bool ref = ref true
  let show_exitval : bool ref = ref false
end

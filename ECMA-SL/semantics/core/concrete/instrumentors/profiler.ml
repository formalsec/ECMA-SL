module ExecutionTime = struct
  type t =
    { start : float
    ; stop : float
    ; diff : float
    }

  let create () : t = { start = -1.0; stop = -1.0; diff = -1.0 } [@@inline]
  let start (ts : t) : t = { ts with start = Sys.time () } [@@inline]
  let diff (ts : t) : t = { ts with diff = ts.stop -. ts.start } [@@inline]
  let stop (ts : t) : t = diff @@ { ts with stop = Sys.time () } [@@inline]
end

module MemoryUsage = struct
  type t =
    { heap_n : int
    ; heap_sz : int
    }

  let create () : t = { heap_n = 0; heap_sz = 0 } [@@inline]
  let calculate (mem : t) : t = mem [@@inline]
  (* TODO: calculate the size of the heap *)
end

module ProgCounter = struct
  type t =
    { calls : int
    ; stmts : int
    ; exprs : int
    ; objs : int
    }

  type item =
    [ `Call
    | `Stmt
    | `Expr
    | `Obj
    ]

  let create () : t = { calls = 0; stmts = 0; exprs = 0; objs = 0 } [@@inline]

  let count (ctr : t) (item : item) : t =
    match item with
    | `Call -> { ctr with calls = ctr.calls + 1 }
    | `Stmt -> { ctr with stmts = ctr.stmts + 1 }
    | `Expr -> { ctr with exprs = ctr.exprs + 1 }
    | `Obj -> { ctr with objs = ctr.objs + 1 }
end

module type M = sig
  type t

  val initial_state : unit -> t
  val start : t -> t
  val stop : t -> t
  val count : t -> ProgCounter.item -> t
end

module Disable : M = struct
  type t = unit

  let initial_state () : t = ()
  let start (metrics : t) : t = metrics
  let stop (metrics : t) : t = metrics
  let count (metrics : t) (_ : ProgCounter.item) : t = metrics
end

module Time : M = struct
  type t = { exec_time : ExecutionTime.t }

  let initial_state () : t = { exec_time = ExecutionTime.create () }

  let start (metrics : t) : t =
    { exec_time = ExecutionTime.start metrics.exec_time }

  let stop (metrics : t) : t =
    { exec_time = ExecutionTime.stop metrics.exec_time }

  let count (metrics : t) (_ : ProgCounter.item) : t = metrics
end

module Full : M = struct
  type t =
    { exec_time : ExecutionTime.t
    ; mem_usage : MemoryUsage.t
    ; counter : ProgCounter.t
    }

  let initial_state () : t =
    { exec_time = ExecutionTime.create ()
    ; mem_usage = MemoryUsage.create ()
    ; counter = ProgCounter.create ()
    }

  let start (metrics : t) : t =
    { metrics with exec_time = ExecutionTime.start metrics.exec_time }

  let stop (metrics : t) : t =
    { metrics with
      exec_time = ExecutionTime.stop metrics.exec_time
    ; mem_usage = MemoryUsage.calculate metrics.mem_usage
    }

  let count (metrics : t) (item : ProgCounter.item) : t =
    { metrics with counter = ProgCounter.count metrics.counter item }
end

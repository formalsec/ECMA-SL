type stmt_eval = Monitor.stmt_eval

module type M = sig
  module Tracer : Tracer.M
  module Debugger : Debugger.M
  module Profiler : Profiler.M
  module Monitor : Monitor.M

  type t =
    { db : Debugger.t
    ; pf : Profiler.t
    ; mon : Monitor.t
    }

  val initial_state : unit -> t
  val cleanup : t -> unit
end

module Default
    (Tr : Tracer.M)
    (Db : Debugger.M)
    (Pf : Profiler.M)
    (Mon : Monitor.M) : M = struct
  module Tracer = Tr
  module Debugger = Db
  module Profiler = Pf
  module Monitor = Mon

  type t =
    { db : Debugger.t
    ; pf : Profiler.t
    ; mon : Monitor.t
    }

  let initial_state () : t =
    { db = Debugger.initial_state ()
    ; pf = Profiler.initial_state ()
    ; mon = Monitor.initial_state ()
    }

  let cleanup (inst : t) : unit = Debugger.cleanup inst.db
end

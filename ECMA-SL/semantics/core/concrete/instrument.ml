type stmt_eval = Monitor.stmt_eval

module type M = sig
  module Tracer : Tracer.M
  module Debugger : Debugger.M
  module Profiler : Profiler.M
  module Monitor : Monitor.M

  type t =
    { db : Debugger.t
    ; pf : Profiler.t
    ; mon_state : Monitor.state
    ; mon_label : Monitor.sl_label
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
    ; mon_state : Monitor.state
    ; mon_label : Monitor.sl_label
    }

  let initial_state () : t =
    { db = Debugger.initial_state ()
    ; pf = Profiler.initial_state ()
    ; mon_state = Monitor.initial_state ()
    ; mon_label = Monitor.initial_label ()
    }

  let cleanup (inst : t) : unit = Debugger.cleanup inst.db
end

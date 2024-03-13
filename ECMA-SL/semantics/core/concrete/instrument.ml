type stmt_eval = Monitor.stmt_eval

module type M = sig
  module Tracer : Tracer.M
  module Debugger : Debugger.M
  module Monitor : Monitor.M

  type t =
    { tr : unit
    ; db : Debugger.t
    ; mon_state : Monitor.state
    ; mon_label : Monitor.sl_label
    }

  val intial_state : unit -> t
end

module Default (Tr : Tracer.M) (Db : Debugger.M) (Mon : Monitor.M) : M = struct
  module Tracer = Tr
  module Debugger = Db
  module Monitor = Mon

  type t =
    { tr : unit
    ; db : Debugger.t
    ; mon_state : Monitor.state
    ; mon_label : Monitor.sl_label
    }

  let intial_state () : t =
    { tr = ()
    ; db = Debugger.intial_state ()
    ; mon_state = Monitor.initial_state ()
    ; mon_label = Monitor.initial_label ()
    }
end

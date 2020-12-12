module type M = sig 

type state_t

type sl

type monitor_return = | MReturn of state_t
                      | MFail of (state_t * string)

val eval_small_step : state_t -> sl SecLabel.t -> monitor_return 

val initial_monitor_state : unit -> state_t

val parse_lvl : string -> sl 

end
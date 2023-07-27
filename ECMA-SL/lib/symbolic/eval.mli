exception Crash of Source.region * string
exception Invalid_arg of Source.region * string

val main : Sym_state.P.env -> string -> unit
(** [main prog f] *)

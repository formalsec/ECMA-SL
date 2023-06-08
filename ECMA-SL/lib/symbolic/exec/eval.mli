exception Crash of Source.region * string
exception Invalid_arg of Source.region * string

val analyse : Prog.t -> State.func -> string -> State.config list
(** [analyse prog f policy] Analyse a program starting in function [f] *)

val main : Prog.t -> State.func -> unit
(** [main prog f] *)

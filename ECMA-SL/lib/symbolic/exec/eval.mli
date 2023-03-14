exception Crash of Source.region * string
exception Invalid_arg of Source.region * string

val analyse : Prog.t -> State.func -> Report.t
(** [analyse prog f] Analyse a program starting in function [f] *)

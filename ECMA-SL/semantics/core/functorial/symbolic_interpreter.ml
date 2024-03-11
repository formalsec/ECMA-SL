open EslSyntax

exception Crash of Source.region * string
exception Invalid_arg of Source.region * string

include Interpreter_functor.Make (Symbolic.P)

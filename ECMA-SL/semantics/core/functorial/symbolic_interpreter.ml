open EslSyntax

exception Crash of Source.at * string
exception Invalid_arg of Source.at * string

include Interpreter_functor.Make (Symbolic.P)

exception Crash of Source.region * string
exception Invalid_arg of Source.region * string

module Eval = Eval_functor.Make(State.P)

let main = Eval.main

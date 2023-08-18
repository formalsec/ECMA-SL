exception Crash of Source.region * string
exception Invalid_arg of Source.region * string

module S = Eval_functor.Make (Sym_state.P)

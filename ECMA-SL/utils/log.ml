let on_debug = ref false
let debug1 fmt a = if !on_debug then Fmt.eprintf fmt a
let debug2 fmt a b = if !on_debug then Fmt.eprintf fmt a b
let warn fmt = Fmt.eprintf fmt
let err fmt = Fmt.kasprintf failwith fmt
let app fmt = Fmt.printf fmt

let on_err f = function
  | Ok v -> v
  | Error (`Msg s) ->
    warn "%s@." s;
    f ()

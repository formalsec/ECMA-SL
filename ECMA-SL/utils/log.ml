let on_debug = ref false
let debug1 fmt a = if !on_debug then Fmt.eprintf fmt a
let debug2 fmt a b = if !on_debug then Fmt.eprintf fmt a b
let warn fmt = Format.eprintf fmt
let err fmt = Format.kasprintf failwith fmt

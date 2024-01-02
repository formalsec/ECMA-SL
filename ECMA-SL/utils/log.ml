let on_debug = ref false

let debug fmt =
  if !on_debug then Format.eprintf fmt
  else Format.ifprintf Format.err_formatter fmt

let err fmt = Format.eprintf fmt

open Core

let print_endline (s : string lazy_t) : unit =
  if !Flags.verbose then printf "[verb] %s\n" (Lazy.force s)

let set_silent () : unit = Flags.verbose := false
let set_verbose () : unit = Flags.verbose := true

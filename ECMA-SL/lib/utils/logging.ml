let print_endline (s : string lazy_t) : unit =
  if !Flags.verbose then print_endline ("[verb] " ^ (Lazy.force s))

let set_silent () : unit = Flags.verbose := false
let set_verbose () : unit = Flags.verbose := true

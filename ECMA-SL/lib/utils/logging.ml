let print (s : string lazy_t) : unit =
  if !Config.verbose then Format.printf "[verb] %s@." (Lazy.force s)

let print_endline (s : string lazy_t) : unit =
  if !Config.verbose then Format.printf "[verb] %s@." (Lazy.force s)

let set_silent () : unit = Config.verbose := false
let set_verbose () : unit = Config.verbose := true

let on_debug = ref false

let debug s : unit = if !on_debug then Format.printf "%s@." (Lazy.force s)

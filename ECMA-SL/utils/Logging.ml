type t =
  | VERBOSE
  | SILENT


let default = ref VERBOSE

let print_endline (s : string lazy_t) : unit =
  match !default with
  | VERBOSE -> Printf.printf "%s\n" (Lazy.force s)
  | SILENT  -> ()

let set_silent () : unit =
  default := SILENT

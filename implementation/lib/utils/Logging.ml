type t =
  | VERBOSE
  | SILENT


let default = ref VERBOSE

let print_endline (s : string) : unit =
  match !default with
  | VERBOSE -> Printf.printf "%s\n" s
  | SILENT  -> ()

let print_string (s : string) : unit =
  match !default with
  | VERBOSE -> Printf.printf "%s" s
  | SILENT  -> ()

let set_silent () : unit =
  default := SILENT

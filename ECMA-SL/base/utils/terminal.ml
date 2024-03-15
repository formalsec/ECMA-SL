let is_terminal (fdesc : Unix.file_descr) : bool = Unix.isatty fdesc

let colored () : bool =
  let ic = Unix.open_process_in "tput colors" in
  let colors = int_of_string (input_line ic) in
  close_in ic;
  colors >= 256

let width () : int =
  let ic = Unix.open_process_in "tput cols" in
  let width = int_of_string (input_line ic) in
  close_in ic;
  width

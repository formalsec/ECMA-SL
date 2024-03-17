module Config = struct
  let min_width = 60
  let max_width = 9999
end

let is_terminal (fdesc : Unix.file_descr) : bool = Unix.isatty fdesc

let colored () : bool =
  let ic = Unix.open_process_in "tput colors" in
  let colors = int_of_string (input_line ic) in
  close_in ic;
  colors >= 256

let width (fdesc : Unix.file_descr) : int =
  if is_terminal fdesc then (
    let ic = Unix.open_process_in "tput cols" in
    let width = int_of_string (input_line ic) in
    close_in ic;
    max width Config.min_width )
  else Config.max_width

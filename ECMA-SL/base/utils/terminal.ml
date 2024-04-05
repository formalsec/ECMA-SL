module Config = struct
  let required_colors = 256
  let min_width = 40
  let min_height = 1
  let max_width = Int.max_int
  let max_height = Int.max_int
end

let terminal_cmd (fdesc : Unix.file_descr) (cmd : string) : string option =
  if Unix.isatty fdesc then
    let ic = Unix.open_process_in cmd in
    let finally () = close_in ic in
    let execute () = input_line ic in
    Some (Fun.protect ~finally execute)
  else None

let colored (fdesc : Unix.file_descr) : bool =
  let cmd = terminal_cmd fdesc "tput colors" in
  let colored' res = int_of_string res >= Config.required_colors in
  Option.fold ~none:false ~some:colored' cmd

let width (fdesc : Unix.file_descr) : int =
  let cmd = terminal_cmd fdesc "tput cols" in
  let width' res = max (int_of_string res) Config.min_width in
  Option.fold ~none:Config.max_width ~some:width' cmd

let height (fdesc : Unix.file_descr) : int =
  let cmd = terminal_cmd fdesc "tput lines" in
  let height' res = max (int_of_string res) Config.min_height in
  Option.fold ~none:Config.max_height ~some:height' cmd

let size (fdesc : Unix.file_descr) : int * int = (width fdesc, height fdesc)

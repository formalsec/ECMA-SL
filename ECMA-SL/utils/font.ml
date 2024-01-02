(* Style *)
let normal = "0 "
let bold = "1"
let faint = "2"
let italic = "3"
let underline = "4"
let blink = "5"
let blinkfast = "6"
let negative = "7"
let conceal = "8"
let strike = "9"

(* Colors *)
let black = "30"
let red = "31"
let green = "32"
let yellow = "33"
let blue = "34"
let purple = "35"
let cyan = "36"
let white = "37"

let clean (text : string) : string =
  let escape_regex = Str.regexp "\027\\[[0-9;]*m" in
  Str.global_replace escape_regex "" text

let format (format_strs : string list) (text : string) : string =
  if text = "" then ""
  else
    let format_str = String.concat ";" format_strs in
    Printf.sprintf "\027[%sm%s\027[0m" format_str text

let fformat (fileDesc : Unix.file_descr) (format_strs : string list)
  (text : string) : string =
  let _is_channel_redirected () =
    try
      let status = Unix.fstat fileDesc in
      status.Unix.st_kind = Unix.S_REG || status.Unix.st_kind = Unix.S_LNK
    with Unix.Unix_error _ -> false
  in
  if not (_is_channel_redirected ()) then format format_strs text else text

let format_err (format_strs : string list) (text : string) : string =
  fformat Unix.stderr format_strs text

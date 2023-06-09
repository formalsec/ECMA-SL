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
  let escapeRegex = Str.regexp "\027\\[[0-9;]*m" in
  Str.global_replace escapeRegex "" text

let format (text : string) (format : string list) : string =
  let format_str =
    "\027[" ^ List.fold_left (fun f s -> f ^ ";" ^ s) "" format ^ "m"
  in
  format_str ^ text ^ "\027[0m"

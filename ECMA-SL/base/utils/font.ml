module Config = struct
  let colored = ref true
  let supported_stdout = Terminal.colored Unix.stdout
  let supported_stderr = Terminal.colored Unix.stderr
end

type t = t' list

and t' =
  | Normal
  | Bold
  | Faint
  | Italic
  | Underline
  | Blink
  | Blinkfast
  | Negative
  | Conceal
  | Strike
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Purple
  | Cyan
  | White

let colored (fdesc : Unix.file_descr option) (fmt : Fmt.t) : bool =
  let supported_fdesc fdesc =
    if fdesc == Unix.stdout then Config.supported_stdout
    else if fdesc == Unix.stderr then Config.supported_stderr
    else false
  in
  if not !Config.colored then false
  else
    Option.fold ~none:true ~some:supported_fdesc fdesc
    && (fmt == Fmt.std_formatter || fmt == Fmt.err_formatter)

let clean (text : string) : string =
  let escape_regex = Str.regexp "\027\\[[0-9;]*m" in
  Str.global_replace escape_regex "" text

let pp_code (fmt : Fmt.t) (font_el : t') : unit =
  let open Fmt in
  match font_el with
  | Normal -> pp_str fmt "0"
  | Bold -> pp_str fmt "1"
  | Faint -> pp_str fmt "2"
  | Italic -> pp_str fmt "3"
  | Underline -> pp_str fmt "4"
  | Blink -> pp_str fmt "5"
  | Blinkfast -> pp_str fmt "6"
  | Negative -> pp_str fmt "7"
  | Conceal -> pp_str fmt "8"
  | Strike -> pp_str fmt "9"
  | Black -> pp_str fmt "30"
  | Red -> pp_str fmt "31"
  | Green -> pp_str fmt "32"
  | Yellow -> pp_str fmt "33"
  | Blue -> pp_str fmt "34"
  | Purple -> pp_str fmt "35"
  | Cyan -> pp_str fmt "36"
  | White -> pp_str fmt "37"

let pp_font (fmt : Fmt.t) (font : t) : unit =
  Fmt.(fprintf fmt "\027[%am" (pp_lst ";" pp_code) font)

let pp ?(fdesc : Unix.file_descr option = None) (font : t)
  (pp_el : Fmt.t -> 'a -> unit) (fmt : Fmt.t) (el : 'a) : unit =
  let open Fmt in
  if not (colored fdesc fmt) then fprintf fmt "%a" pp_el el
  else fprintf fmt "%a%a%a" pp_font font pp_el el pp_font [ Normal ]

let str ?(fdesc : Unix.file_descr option = None) (font : t)
  (pp_el : Fmt.t -> 'a -> unit) (el : 'a) : string =
  Fmt.asprintf "%a" (pp ~fdesc font pp_el) el

let pp_text ?(fdesc : Unix.file_descr option = None) (font : t) (fmt : Fmt.t)
  (s : string) =
  pp ~fdesc font Fmt.pp_print_string fmt s

let str_text ?(fdesc : Unix.file_descr option = None) (font : t) (s : string) =
  Fmt.asprintf "%a" (pp_text ~fdesc font) s

let pp_none _font pp_el fmt el = pp_el fmt el
let pp_out font pp_el fmt el = pp ~fdesc:(Some Unix.stdout) font pp_el fmt el
let pp_err font pp_el fmt el = pp ~fdesc:(Some Unix.stderr) font pp_el fmt el
let str_out font pp_el el = str ~fdesc:(Some Unix.stdout) font pp_el el
let str_err font pp_el el = str ~fdesc:(Some Unix.stderr) font pp_el el
let pp_text_out font fmt s = pp_text ~fdesc:(Some Unix.stdout) font fmt s
let pp_text_err font fmt s = pp_text ~fdesc:(Some Unix.stderr) font fmt s
let str_text_out font s = str_text ~fdesc:(Some Unix.stdout) font s
let str_text_err font s = str_text ~fdesc:(Some Unix.stderr) font s

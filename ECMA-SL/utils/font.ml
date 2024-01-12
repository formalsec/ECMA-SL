type t =
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

let clean (text : string) : string =
  let escape_regex = Str.regexp "\027\\[[0-9;]*m" in
  Str.global_replace escape_regex "" text

let pp_code (fmt : Fmt.formatter) (font_el : t) : unit =
  let open Fmt in
  match font_el with
  | Normal -> fprintf fmt "0"
  | Bold -> fprintf fmt "1"
  | Faint -> fprintf fmt "2"
  | Italic -> fprintf fmt "3"
  | Underline -> fprintf fmt "4"
  | Blink -> fprintf fmt "5"
  | Blinkfast -> fprintf fmt "6"
  | Negative -> fprintf fmt "7"
  | Conceal -> fprintf fmt "8"
  | Strike -> fprintf fmt "9"
  | Black -> fprintf fmt "30"
  | Red -> fprintf fmt "31"
  | Green -> fprintf fmt "32"
  | Yellow -> fprintf fmt "33"
  | Blue -> fprintf fmt "34"
  | Purple -> fprintf fmt "35"
  | Cyan -> fprintf fmt "36"
  | White -> fprintf fmt "37"

let pp_font (fmt : Fmt.formatter) (font : t list) : unit =
  let open Fmt in
  let pp_sep seq fmt () = pp_print_string fmt seq in
  let pp_lst seq pp fmt lst = pp_print_list ~pp_sep:(pp_sep seq) pp fmt lst in
  fprintf fmt "%a" (pp_lst ";" pp_code) font

let pp ?(fdesc : Unix.file_descr option = None) (font : t list)
  (pp_el : Fmt.formatter -> 'a -> unit) (fmt : Fmt.formatter) (el : 'a) : unit =
  let open Fmt in
  if not !Config.Common.colored then fprintf fmt "%a" pp_el el
  else
    match fdesc with
    | Some fdesc' when not Unix.(isatty fdesc') -> fprintf fmt "%a" pp_el el
    | _ -> fprintf fmt "\027[%am%a\027[0m" pp_font font pp_el el

let format ?(fdesc : Unix.file_descr option = None) (font : t list)
  (pp_el : Fmt.formatter -> 'a -> unit) (el : 'a) : string =
  Fmt.asprintf "%a" (pp ~fdesc font pp_el) el

let pp_out font pp_el fmt el = pp ~fdesc:(Some Unix.stdout) font pp_el fmt el
let pp_err font pp_el fmt el = pp ~fdesc:(Some Unix.stderr) font pp_el fmt el
let format_out font pp_el el = format ~fdesc:(Some Unix.stdout) font pp_el el
let format_err font pp_el el = format ~fdesc:(Some Unix.stderr) font pp_el el

let str_pp ?(fdesc : Unix.file_descr option = None) (font : t list)
  (fmt : Fmt.formatter) (s : string) =
  pp ~fdesc font Fmt.pp_print_string fmt s

let str_format ?(fdesc : Unix.file_descr option = None) (font : t list)
  (s : string) =
  Fmt.asprintf "%a" (str_pp ~fdesc font) s

let str_pp_out font fmt s = str_pp ~fdesc:(Some Unix.stdout) font fmt s
let str_pp_err font fmt s = str_pp ~fdesc:(Some Unix.stderr) font fmt s
let str_format_out font s = str_format ~fdesc:(Some Unix.stdout) font s
let str_format_err font s = str_format ~fdesc:(Some Unix.stderr) font s

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

let colored (fdesc : Unix.file_descr option) (ppf : Fmt.t) : bool =
  let supported_fdesc fdesc =
    if fdesc == Unix.stdout then Config.supported_stdout
    else if fdesc == Unix.stderr then Config.supported_stderr
    else false
  in
  if not !Config.colored then false
  else
    Option.fold ~none:true ~some:supported_fdesc fdesc
    && (ppf == Fmt.std_formatter || ppf == Fmt.err_formatter)

let clean (text : string) : string =
  let escape_regex = Str.regexp "\027\\[[0-9;]*m" in
  Str.global_replace escape_regex "" text

let pp_code (ppf : Fmt.t) (font_el : t') : unit =
  let open Fmt in
  match font_el with
  | Normal -> pp_str ppf "0"
  | Bold -> pp_str ppf "1"
  | Faint -> pp_str ppf "2"
  | Italic -> pp_str ppf "3"
  | Underline -> pp_str ppf "4"
  | Blink -> pp_str ppf "5"
  | Blinkfast -> pp_str ppf "6"
  | Negative -> pp_str ppf "7"
  | Conceal -> pp_str ppf "8"
  | Strike -> pp_str ppf "9"
  | Black -> pp_str ppf "30"
  | Red -> pp_str ppf "31"
  | Green -> pp_str ppf "32"
  | Yellow -> pp_str ppf "33"
  | Blue -> pp_str ppf "34"
  | Purple -> pp_str ppf "35"
  | Cyan -> pp_str ppf "36"
  | White -> pp_str ppf "37"

let pp_font (ppf : Fmt.t) (font : t) : unit =
  Fmt.(fprintf ppf "\027[%am" (pp_lst ";" pp_code) font)

let pp ?(fdesc : Unix.file_descr option = None) (font : t)
  (pp_el : Fmt.t -> 'a -> unit) (ppf : Fmt.t) (el : 'a) : unit =
  let open Fmt in
  if not (colored fdesc ppf) then fprintf ppf "%a" pp_el el
  else fprintf ppf "%a%a%a" pp_font font pp_el el pp_font [ Normal ]

let str ?(fdesc : Unix.file_descr option = None) (font : t)
  (pp_el : Fmt.t -> 'a -> unit) (el : 'a) : string =
  Fmt.asprintf "%a" (pp ~fdesc font pp_el) el

let pp_text ?(fdesc : Unix.file_descr option = None) (font : t) (ppf : Fmt.t)
  (s : string) =
  pp ~fdesc font Fmt.pp_print_string ppf s

let str_text ?(fdesc : Unix.file_descr option = None) (font : t) (s : string) =
  Fmt.asprintf "%a" (pp_text ~fdesc font) s

let pp_none _font pp_el ppf el = pp_el ppf el
let pp_out font pp_el ppf el = pp ~fdesc:(Some Unix.stdout) font pp_el ppf el
let pp_err font pp_el ppf el = pp ~fdesc:(Some Unix.stderr) font pp_el ppf el
let str_out font pp_el el = str ~fdesc:(Some Unix.stdout) font pp_el el
let str_err font pp_el el = str ~fdesc:(Some Unix.stderr) font pp_el el
let pp_text_out font ppf s = pp_text ~fdesc:(Some Unix.stdout) font ppf s
let pp_text_err font ppf s = pp_text ~fdesc:(Some Unix.stderr) font ppf s
let str_text_out font s = str_text ~fdesc:(Some Unix.stdout) font s
let str_text_err font s = str_text ~fdesc:(Some Unix.stderr) font s

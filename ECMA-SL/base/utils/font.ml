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
  match font_el with
  | Normal -> Fmt.pp_str ppf "0"
  | Bold -> Fmt.pp_str ppf "1"
  | Faint -> Fmt.pp_str ppf "2"
  | Italic -> Fmt.pp_str ppf "3"
  | Underline -> Fmt.pp_str ppf "4"
  | Blink -> Fmt.pp_str ppf "5"
  | Blinkfast -> Fmt.pp_str ppf "6"
  | Negative -> Fmt.pp_str ppf "7"
  | Conceal -> Fmt.pp_str ppf "8"
  | Strike -> Fmt.pp_str ppf "9"
  | Black -> Fmt.pp_str ppf "30"
  | Red -> Fmt.pp_str ppf "31"
  | Green -> Fmt.pp_str ppf "32"
  | Yellow -> Fmt.pp_str ppf "33"
  | Blue -> Fmt.pp_str ppf "34"
  | Purple -> Fmt.pp_str ppf "35"
  | Cyan -> Fmt.pp_str ppf "36"
  | White -> Fmt.pp_str ppf "37"

let pp_font (ppf : Fmt.t) (font : t) : unit =
  Fmt.(format ppf "\027[%am" (pp_lst !>";" pp_code) font)

let pp ?(fdesc : Unix.file_descr option = None) (font : t)
  (pp_el : Fmt.t -> 'a -> unit) (ppf : Fmt.t) (el : 'a) : unit =
  if not (colored fdesc ppf) then pp_el ppf el
  else Fmt.format ppf "%a%a%a" pp_font font pp_el el pp_font [ Normal ]

let str ?(fdesc : Unix.file_descr option = None) (font : t)
  (pp_el : Fmt.t -> 'a -> unit) (el : 'a) : string =
  Fmt.str "%a" (pp ~fdesc font pp_el) el

let pp_text ?(fdesc : Unix.file_descr option = None) (font : t) (ppf : Fmt.t)
  (s : string) =
  pp ~fdesc font Fmt.pp_str ppf s

let str_text ?(fdesc : Unix.file_descr option = None) (font : t) (s : string) =
  Fmt.str "%a" (pp_text ~fdesc font) s

let pp_none _font pp_el ppf el = pp_el ppf el
let pp_out font pp_el ppf el = pp ~fdesc:(Some Unix.stdout) font pp_el ppf el
let pp_err font pp_el ppf el = pp ~fdesc:(Some Unix.stderr) font pp_el ppf el
let str_out font pp_el el = str ~fdesc:(Some Unix.stdout) font pp_el el
let str_err font pp_el el = str ~fdesc:(Some Unix.stderr) font pp_el el
let pp_text_out font ppf s = pp_text ~fdesc:(Some Unix.stdout) font ppf s
let pp_text_err font ppf s = pp_text ~fdesc:(Some Unix.stderr) font ppf s
let str_text_out font s = str_text ~fdesc:(Some Unix.stdout) font s
let str_text_err font s = str_text ~fdesc:(Some Unix.stderr) font s

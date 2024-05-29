module Config = struct
  let colored = ref true
end

let colored (fdesc : Unix.file_descr option) (ppf : Fmt.t) : bool =
  !Config.colored
  && Option.fold ~none:true ~some:Terminal.colored fdesc
  && (ppf == Fmt.std_formatter || ppf == Fmt.err_formatter)

type font_v =
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

type t = font_v list

let pp_font_v (ppf : Fmt.t) (font_v : font_v) : unit =
  match font_v with
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
  Fmt.(format ppf "\027[%am" (pp_lst !>";" pp_font_v) font)

let pp ?(fdesc : Unix.file_descr option = None) (font : t)
  (pp_v : Fmt.t -> 'a -> unit) (ppf : Fmt.t) (v : 'a) : unit =
  if not (colored fdesc ppf) then pp_v ppf v
  else Fmt.format ppf "%a%a%a" pp_font font pp_v v pp_font [ Normal ]

let str ?(fdesc : Unix.file_descr option = None) (font : t)
  (pp_v : Fmt.t -> 'a -> unit) (v : 'a) : string =
  Fmt.str "%a" (pp ~fdesc font pp_v) v

let pp_none _font pp_v ppf v = pp_v ppf v
let pp_out font pp_v ppf v = pp ~fdesc:(Some Unix.stdout) font pp_v ppf v
let pp_err font pp_v ppf v = pp ~fdesc:(Some Unix.stderr) font pp_v ppf v
let str_out font pp_v v = str ~fdesc:(Some Unix.stdout) font pp_v v
let str_err font pp_v v = str ~fdesc:(Some Unix.stderr) font pp_v v
let pp_text_out font ppf s = pp ~fdesc:(Some Unix.stdout) font Fmt.pp_str ppf s
let pp_text_err font ppf s = pp ~fdesc:(Some Unix.stderr) font Fmt.pp_str ppf s
let str_text_out font s = str ~fdesc:(Some Unix.stdout) font Fmt.pp_str s
let str_text_err font s = str ~fdesc:(Some Unix.stderr) font Fmt.pp_str s

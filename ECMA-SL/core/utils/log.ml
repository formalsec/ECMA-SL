module Config = struct
  let warns = ref true
  let debugs = ref false
end

open Fmt

let make_log ?(header : bool = true) ?(font : Font.t = [ Font.Normal ])
  (fdesc : Unix.file_descr) (fmt : ('a, t, unit, unit) format4) : 'a =
  let reset = [ Font.Normal ] in
  let pp_font = Font.pp_font_safe ~fdesc:(Some fdesc) in
  let hdr = if header then "[ecma-sl] " else "" in
  let ppf = formatter_of_out_channel (Unix.out_channel_of_descr fdesc) in
  kdprintf
    (fun fmt -> fprintf ppf "%a%s%t%a@." pp_font font hdr fmt pp_font reset)
    fmt

let conditional_log test logger fmt =
  if test then logger fmt else ifprintf std_formatter fmt

let log ?(test = true) ?(header = true) ?(font = [ Font.Normal ]) fmt =
  conditional_log test (make_log ~header ~font Unix.stdout) fmt

let elog ?(test = true) ?(header = true) ?(font = [ Font.Normal ]) fmt =
  conditional_log test (make_log ~header ~font Unix.stderr) fmt

let err fmt = kasprintf failwith fmt
let warn fmt = elog ~test:!Config.warns ~font:[ Font.Yellow ] fmt
let debug fmt = elog ~test:!Config.debugs ~font:[ Font.Cyan ] fmt
let app fmt = log ~header:false fmt

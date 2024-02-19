open Fmt

let log ?(header = true) msg_fmt =
  let header_str = if header then "[ecma-sl] " else "" in
  kdprintf (eprintf "%s%t@." header_str) msg_fmt

let cond_log cond msg_fmt =
  if cond then log msg_fmt else ifprintf std_formatter msg_fmt

let debug debug_fmt = cond_log !Config.Common.debugs debug_fmt
let warn warn_fmt = cond_log !Config.Common.warns warn_fmt
let err fmt = kasprintf failwith fmt
let app fmt = printf fmt

let on_err f = function
  | Ok v -> v
  | Error (`Msg s) ->
    warn "%s" s;
    f ()

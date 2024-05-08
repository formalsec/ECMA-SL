open Fmt

module Config = struct
  let warns : bool ref = ref true
  let debugs : bool ref = ref false
  let out_ppf : Fmt.t ref = ref std_formatter
  let err_ppf : Fmt.t ref = ref err_formatter
end

module EslLog = struct
  let pp_font ppf =
    if ppf == !Config.out_ppf then Font.pp_out
    else if ppf == !Config.err_ppf then Font.pp_err
    else Font.pp_none

  let mk ?(font : Font.t = [ Font.Normal ]) ?(nl : bool = false) (ppf : Fmt.t)
    (fmt : ('a, t, unit, unit) format4) : 'a =
    let pp_nl ppf nl = if nl then fprintf ppf "@;" else () in
    let pp_log ppf fmt = fprintf ppf "%a[ecma-sl] %t" pp_nl nl fmt in
    kdprintf (fun fmt -> fprintf ppf "%a@." ((pp_font ppf) font pp_log) fmt) fmt

  let conditional (test : bool) ?(font : Font.t option) (ppf : Fmt.t)
    (fmt : ('a, t, unit) format) =
    if test then (mk ?font ppf) fmt else ifprintf std_formatter fmt
end

let out fmt = kdprintf (fprintf !Config.out_ppf "%t") fmt [@@inline]
let err fmt = kdprintf (fprintf !Config.err_ppf "%t") fmt [@@inline]
let fail fmt = kasprintf failwith fmt [@@inline]
let esl ?(nl = false) fmt = EslLog.mk ~nl !Config.out_ppf fmt [@@inline]
let error fmt = EslLog.mk ~font:[ Red ] !Config.err_ppf fmt [@@inline]

let warn fmt =
  EslLog.conditional !Config.warns ~font:[ Yellow ] !Config.err_ppf fmt
[@@inline]

let debug fmt =
  EslLog.conditional !Config.debugs ~font:[ Cyan ] !Config.err_ppf fmt
[@@inline]

module Redirect = struct
  type t =
    { old_out : Fmt.t
    ; old_err : Fmt.t
    ; new_out : Buffer.t option
    ; new_err : Buffer.t option
    }

  type capture_mode =
    | Out
    | Err
    | OutErr
    | Shared

  let capture (ppf_ref : Fmt.t ref) (buffer : Buffer.t) : Buffer.t =
    ppf_ref := Fmt.formatter_of_buffer buffer;
    buffer

  let capture_to ~(out : Buffer.t option) ~(err : Buffer.t option) : t =
    let (old_out, old_err) = (!Config.out_ppf, !Config.err_ppf) in
    let new_out = Option.map (capture Config.out_ppf) out in
    let new_err = Option.map (capture Config.err_ppf) err in
    { old_out; old_err; new_out; new_err }

  let capture (mode : capture_mode) : t =
    let buffer () = Some (Buffer.create 1024) in
    match mode with
    | Out -> capture_to ~out:(buffer ()) ~err:None
    | Err -> capture_to ~out:None ~err:(buffer ())
    | OutErr -> capture_to ~out:(buffer ()) ~err:(buffer ())
    | Shared ->
      let streams = capture_to ~out:(buffer ()) ~err:None in
      let old_err = !Config.err_ppf in
      Config.err_ppf := !Config.out_ppf;
      { streams with old_err; new_err = None }

  let restore ?(log : bool = false) (streams : t) : unit =
    let open Fmt in
    let log ppf buf = if log then fprintf ppf "%s@?" (Buffer.contents buf) in
    Config.out_ppf := streams.old_out;
    Config.err_ppf := streams.old_err;
    Option.fold ~none:() ~some:(log !Config.out_ppf) streams.new_out;
    Option.fold ~none:() ~some:(log !Config.err_ppf) streams.new_err
end

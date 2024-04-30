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
    let pp_nl ppf nl = if nl then fprintf ppf "\n" else () in
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
    ; new_out : (out_channel * string) option
    ; new_err : (out_channel * string) option
    }

  type capture_mode =
    | Null
    | Out
    | Err
    | OutErr
    | Shared

  let stream (ppf_ref : Fmt.t ref) (fstream : string) :
    (out_channel * string) option =
    let fd = Unix.openfile fstream [ O_WRONLY ] 0o666 in
    let oc = Unix.out_channel_of_descr fd in
    ppf_ref := formatter_of_out_channel oc;
    Some (oc, fstream)

  let close (log : bool) (ppf : Fmt.t) ((oc, fstream) : out_channel * string) :
    unit =
    close_out oc;
    if log then fprintf ppf "%s@?" (Io.read_file fstream)

  let capture_to ~(out : string option) ~(err : string option) : t =
    let (old_out, old_err) = (!Config.out_ppf, !Config.err_ppf) in
    let new_out = Option.fold ~none:None ~some:(stream Config.out_ppf) out in
    let new_err = Option.fold ~none:None ~some:(stream Config.err_ppf) err in
    { old_out; old_err; new_out; new_err }

  let capture (mode : capture_mode) : t =
    let temp_file ext = Some (Filename.temp_file "ecma-sl" ("logger_" ^ ext)) in
    match mode with
    | Null -> capture_to ~out:(Some Filename.null) ~err:(Some Filename.null)
    | Out -> capture_to ~out:(temp_file "out") ~err:None
    | Err -> capture_to ~out:None ~err:(temp_file "err")
    | OutErr -> capture_to ~out:(temp_file "out") ~err:(temp_file "err")
    | Shared ->
      let streams = capture_to ~out:(temp_file "shared") ~err:None in
      let old_err = !Config.err_ppf in
      Config.err_ppf := !Config.out_ppf;
      { streams with old_err; new_err = None }

  let restore ?(log : bool = false) (streams : t) : unit =
    Config.out_ppf := streams.old_out;
    Config.err_ppf := streams.old_err;
    ignore (Option.map (close log !Config.out_ppf) streams.new_out);
    ignore (Option.map (close log !Config.err_ppf) streams.new_err)
end

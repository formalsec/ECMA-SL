open Fmt

let ppf_of_fd (fd : Unix.file_descr) : Fmt.t =
  formatter_of_out_channel (Unix.out_channel_of_descr fd)

module Config = struct
  let warns : bool ref = ref true
  let debugs : bool ref = ref false
  let out_ppf : Fmt.t ref = ref (ppf_of_fd Unix.stdout)
  let err_ppf : Fmt.t ref = ref (ppf_of_fd Unix.stderr)
end

module EslLog = struct
  let pp_font ppf =
    if ppf == !Config.out_ppf then Font.pp_out
    else if ppf == !Config.err_ppf then Font.pp_err
    else Font.pp_none

  let mk ?(font : Font.t option = None) (ppf : Fmt.t)
    (fmt : ('a, t, unit, unit) format4) : 'a =
    let font = Option.value ~default:[ Font.Normal ] font in
    let pp_log ppf fmt = fprintf ppf "[ecma-sl] %t" fmt in
    kdprintf (fun fmt -> fprintf ppf "%a@." ((pp_font ppf) font pp_log) fmt) fmt

  let conditional (test : bool) ?(font : Font.t option = None) (ppf : Fmt.t)
    (fmt : ('a, t, unit) format) =
    if test then (mk ~font ppf) fmt else ifprintf std_formatter fmt
end

let out fmt = kdprintf (fprintf !Config.out_ppf "%t") fmt [@@inline]
let err fmt = kdprintf (fprintf !Config.err_ppf "%t") fmt [@@inline]
let fail fmt = kasprintf failwith fmt [@@inline]
let error fmt = EslLog.mk ~font:(Some [ Red ]) !Config.err_ppf fmt [@@inline]

let warn fmt =
  EslLog.conditional !Config.warns ~font:(Some [ Yellow ]) !Config.err_ppf fmt
[@@inline]

let debug fmt =
  EslLog.conditional !Config.debugs ~font:(Some [ Cyan ]) !Config.err_ppf fmt
[@@inline]

module Redirect = struct
  type t =
    { out : Fmt.t
    ; err : Fmt.t
    ; fout : string option
    ; ferr : string option
    }

  type capture_mode =
    | Null
    | Out
    | Err
    | OutErr
    | Shared

  let stream (ppf_ref : Fmt.t ref) (fsteam : string) : Fmt.t * string option =
    let old_ppf = !ppf_ref in
    let fd = Unix.openfile fsteam [ O_WRONLY ] 0o666 in
    ppf_ref := ppf_of_fd fd;
    (old_ppf, Some fsteam)

  let show (ppf : Fmt.t) (fsteam : string) : unit =
    fprintf ppf "%s@?" (Io.read_file fsteam)

  let capture_to ~(out : string option) ~(err : string option) : t =
    let open Config in
    let (out_dflt, err_dflt) = ((!out_ppf, None), (!err_ppf, None)) in
    let (out, fout) = Option.fold ~none:out_dflt ~some:(stream out_ppf) out in
    let (err, ferr) = Option.fold ~none:err_dflt ~some:(stream err_ppf) err in
    { out; err; fout; ferr }

  let capture (mode : capture_mode) : t =
    let temp_file ext = Some (Filename.temp_file "ecma-sl" ("logger_" ^ ext)) in
    match mode with
    | Null -> capture_to ~out:(Some Filename.null) ~err:(Some Filename.null)
    | Out -> capture_to ~out:(temp_file "out") ~err:None
    | Err -> capture_to ~out:None ~err:(temp_file "err")
    | OutErr -> capture_to ~out:(temp_file "out") ~err:(temp_file "err")
    | Shared ->
      let streams = capture_to ~out:(temp_file "shared") ~err:None in
      let err_ppf = !Config.err_ppf in
      Config.err_ppf := !Config.out_ppf;
      { streams with err = err_ppf; ferr = None }

  let restore ?(log : bool = false) (streams : t) : unit =
    Config.out_ppf := streams.out;
    Config.err_ppf := streams.err;
    if log then (
      ignore (Option.map (show !Config.out_ppf) streams.fout);
      ignore (Option.map (show !Config.err_ppf) streams.ferr) )
end

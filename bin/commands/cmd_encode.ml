open Ecma_sl

type options =
  { inputs : Fpath.t
  ; output : Fpath.t option
  ; builder : string option
  }

let encode (builder : string option) (input : Fpath.t) (output : Fpath.t option)
  : unit =
  let input' = Fpath.to_string input in
  let output' = Option.map Fpath.to_string output in
  match Bos.OS.Cmd.run (Js2ecmasl.cmd input' output' builder) with
  | Error _ -> raise (Cmd.Command_error Error)
  | Ok _ -> Ecma_sl.Log.debug "File '%s' has been encoded." input'

let run_single (opts : options) (input : Fpath.t) (output : Fpath.t option) =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  encode opts.builder input output

let run (opts : options) : unit =
  Dir.exec (run_single opts) opts.inputs opts.output (Enums.Lang.str CESL)

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Cmd.eval_cmd (fun () -> run opts)

type options =
  { inputs : Fpath.t
  ; output : Fpath.t option
  ; builder : string option
  }

module JS2ECMA_SL = struct
  open Bos_setup

  let set_output (output : string option) (cmd : Cmd.t) : Cmd.t =
    match output with
    | None -> cmd
    | Some output' -> Cmd.(add_args cmd (v "-o" % output'))

  let set_builder (builder : string option) (cmd : Cmd.t) : Cmd.t =
    match builder with
    | None -> cmd
    | Some builder' -> Cmd.(add_args cmd (v "-b" % builder'))

  let cmd (input : string) (output : string option) (builder : string option) :
    Cmd.t =
    Cmd.(v "js2ecma-sl" % "-s" % "-c" % "-i" % input)
    |> set_output output
    |> set_builder builder
end

let encode (builder : string option) (input : Fpath.t) (output : Fpath.t option)
  : unit =
  let input' = Fpath.to_string input in
  let output' = Option.map Fpath.to_string output in
  match Bos_setup.OS.Cmd.run (JS2ECMA_SL.cmd input' output' builder) with
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

type options =
  { input : string
  ; output : string option
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
    Cmd.(v "js2ecma-sl" % "-c" % "-i" % input)
    |> set_output output
    |> set_builder builder
end

let run (opts : options) : unit =
  let (input, output, builder) = (opts.input, opts.output, opts.builder) in
  ignore (Cmd.test_file_ext [ Lang.JS ] input);
  match Bos_setup.OS.Cmd.run (JS2ECMA_SL.cmd input output builder) with
  | Ok _ -> ()
  | Error _ -> raise (Cmd.Command_error Error)

let main (copts : Options.Common.t) (opts : options) : int =
  Options.Common.set copts;
  Cmd.eval_cmd (fun () -> run opts)

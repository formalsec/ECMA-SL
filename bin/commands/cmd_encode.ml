open EslCore

module Options = struct
  type t =
    { inputs : Fpath.t
    ; output : Fpath.t option
    ; builder : string option
    }

  let set_options inputs output builder = { inputs; output; builder }
end

let encode (builder : string option) (input : Fpath.t) (output : Fpath.t option)
  : unit =
  let input' = Fpath.to_string input in
  let output' = Option.map Fpath.to_string output in
  match Bos.OS.Cmd.run (EslJSParser.Api.cmd input' output' builder) with
  | Ok _ -> Log.debug "Sucessfuly encoded file '%a'." Fpath.pp input
  | Error _ -> raise (Cmd.Command_error Error)

let run_single (builder : string option) (input : Fpath.t)
  (output : Fpath.t option) : unit =
  ignore Lang.(resolve_file_lang [ JS ] input);
  encode builder input output

let run (opts : Options.t) : unit =
  Dir.exec (run_single opts.builder) opts.inputs opts.output (Lang.str CESL)

let main () (opts : Options.t) : int = Cmd.eval_cmd (fun () -> run opts)

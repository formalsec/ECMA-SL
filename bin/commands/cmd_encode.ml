open Ecma_sl

module Options = struct
  type t =
    { inputs : Fpath.t list
    ; output : Fpath.t option
    ; builder : string option
    }

  let set (inputs : Fpath.t list) (output : Fpath.t option)
    (builder : string option) : t =
    { inputs; output; builder }
end

let encode (builder : string option) (input : Fpath.t) (output : Fpath.t option)
  : unit Result.t =
  let input' = Fpath.to_string input in
  let output' = Option.map Fpath.to_string output in
  match Bos.OS.Cmd.run (EslJSParser.Api.cmd input' output' builder) with
  | Ok () -> Ok (Log.debug "Sucessfuly encoded file '%a'." Fpath.pp input)
  | Error (`Msg msg) -> Result.error (`Encode msg)

let run_single (builder : string option) (input : Fpath.t)
  (output : Fpath.t option) : unit Result.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  encode builder input output

let run () (opts : Options.t) : unit Result.t =
  Files.exec_multiple (run_single opts.builder) opts.inputs opts.output
    (Enums.Lang.str CESL)

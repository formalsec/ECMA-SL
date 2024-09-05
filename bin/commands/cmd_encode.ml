open Ecma_sl
open Smtml.Syntax.Result

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

let run_single (builder : string option) (_ : Fpath.t) (input : Fpath.t)
  (output : Files.output) : unit Result.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  encode builder input (Files.get_output output)

let run () (opts : Options.t) : unit Result.t =
  let* inputs = Files.generate_input_list opts.inputs in
  Files.process_inputs ~outext:(Enums.Lang.str CESL) (run_single opts.builder)
    inputs opts.output

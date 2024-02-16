open Ecma_sl

type options = { filename : Fpath.t }

let options filename = { filename }

let get_content = function
  | "-" -> Ok (In_channel.input_all stdin)
  | filename ->
    if Sys.file_exists filename then Ok (Io.read_file filename)
    else Error (Format.sprintf "File '%s' does not exist." filename)

let main _copts (opts : options) =
  match get_content @@ Fpath.to_string opts.filename with
  | Error msg ->
    Fmt.eprintf "%s@." msg;
    1
  | Ok data ->
    let eexpr = Parsing_utils.parse_eexpr data in
    Format.printf "Parsed: %a" EExpr.pp eexpr;
    0

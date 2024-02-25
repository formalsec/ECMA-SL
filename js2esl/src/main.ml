open Cmdliner
open Js2esl

let main (filename : string) : int =
  let code =
    match Js_parser.from_file ~filename with
    | Ok ast ->
      let json = Json_translator.program None ast in
      Format.printf "Ast: %s@.%a@." filename
        (Yojson.pretty_print ~std:true)
        json;
      `Success
    | Error err -> err
  in
  Types.int_of_exit_value code

let input_file =
  let doc = "" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv)

let cli =
  let cmd = Term.(const main $ input_file) in
  let info = Cmd.info "js2esl" in
  Cmd.v info cmd

let () = exit @@ Cmd.eval' cli

open Cmdliner

let main (file : string) : int =
  print_endline file;
  0

let input_file =
  let doc = "" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv)

let cli =
  let cmd = Term.(const main $ input_file) in
  let info = Cmd.info "js2esl" in
  Cmd.v info cmd

let () = exit @@ Cmd.eval' cli

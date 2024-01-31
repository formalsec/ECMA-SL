let run (input_file : string) (_output_file : string option)
  (_encode_esl : bool) (_builder_func : string) : unit =
  ignore (Cmd.test_file_ext [ Lang.JS ] input_file);
  print_endline "NOT IMPLEMENTED!"

let main (copts : Options.Common.t) (input_file : string)
  (output_file : string option) (encode_esl : bool) (builder_func : string) :
  int =
  Options.Common.set copts;
  Cmd.eval_cmd (fun () -> run input_file output_file encode_esl builder_func)

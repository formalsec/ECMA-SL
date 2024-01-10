let run (input_file : string) (_output_file : string option)
  (_encode_esl : bool) (_builder_func : string) : unit =
  ignore (Cmd.test_file_lang input_file [ Lang.JS ]);
  print_endline "NOT IMPLEMENTED!"

let main (_copts : Options.common_options) (input_file : string)
  (output_file : string option) (encode_esl : bool) (builder_func : string) :
  int =
  Cmd.eval_cmd (fun () -> run input_file output_file encode_esl builder_func)

let run (input_file : string) (_output_file : string option) (_encode_esl : bool)
  (_builder_func : string) : unit =
  Cmd.test_file_ext input_file [ ".js" ];
 print_endline "NOT IMPLEMENTED!"

let main (_copts : Options.common_options) (input_file : string) (output_file : string option)
  (encode_esl : bool) (builder_func : string) : int =
  let run' () = run input_file output_file encode_esl builder_func in
  Cmd.eval_cmd run'

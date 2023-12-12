let run (input_file : string) (output_file : string option) (encode_esl : bool)
  (builder_func : string) : unit =
  Cmd.test_file_ext input_file [ ".js" ];
  let _ = input_file in
  let _ = output_file in
  let _ = encode_esl in
  let _ = builder_func in
  print_endline "NOT IMPLEMENTED!"

let main (debug : bool) (input_file : string) (output_file : string option)
  (encode_esl : bool) (builder_func : string) : int =
  let _ = debug in
  let run' () = run input_file output_file encode_esl builder_func in
  Cmd.eval_cmd run'

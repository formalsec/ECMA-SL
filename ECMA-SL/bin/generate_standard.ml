let input_file = ref ""
let output_file = ref ""
let json_file = ref ""

let arguments () =
  let usage_msg = "Usage: -i <path> -o <path> [ -j <path> ]" in
  Arg.parse
    [
      ("-i", Arg.String (fun f -> input_file := f), "Input ECMA-SL file");
      ("-j", Arg.String (fun j -> json_file := j), "Input JSON file");
      ("-o", Arg.String (fun o -> output_file := o), "Output HTML file");
    ]
    (fun s -> Printf.printf "Ignored Argument: %s" s)
    usage_msg

let generate_html_doc () : unit =
  let file_contents = Parsing_utils.load_file !input_file in
  let e_prog = Parsing_utils.parse_e_prog !input_file file_contents in
  let html_str = HTMLGenerator.generate !json_file e_prog in
  Io.write_file !output_file html_str;
  print_endline "HTML file generated!"

let run () =
  arguments ();
  if !input_file = "" && !output_file = "" then
    invalid_arg "Missing necessary arguments. Use -h"
  else generate_html_doc ()

let _ = run ()

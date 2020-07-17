
let file = ref ""
let heap_file = ref ""
let mode = ref ""
let out = ref ""
let verb_aux = ref false

(* Argument read function *)
let arguments () =

  let usage_msg = "Usage: -i <path> -mode <c/> -o <path> -v <bool> " in
  Arg.parse
    [
      ("-i", Arg.String (fun f -> file := f), "input file")
    ;("-mode", Arg.String (fun m -> mode := m ), "mode to run: c - Core / p - Plus ")
    ;("-heap", Arg.String(fun f -> heap_file := f), "File with the heap. Program runs against this heap.")
    ;("-o", Arg.String (fun o -> out := o ), "output file")
    ;("-v", Arg.Set verb_aux, "verbose")

    ]
    (fun s -> Printf.printf "Ignored Argument: %s" s)
    usage_msg


let compile_from_plus_to_core () : unit =
  let e_prog_contents = Parsing_Utils.load_file !file in
  let e_prog = Parsing_Utils.parse_e_prog e_prog_contents in
  let c_prog = Compiler.compile_prog e_prog in
  print_endline (Prog.str c_prog)

let core_interpretation () : unit =
  let prog_contents = Parsing_Utils.load_file !file in
  let prog = Parsing_Utils.parse_prog prog_contents in
  let v = Core_Interpreter.eval_prog prog [] [] !mode !out !verb_aux in
  match v with
	|Some z ->	print_string ("MAIN return -> "^(Val.str z))
	| None -> print_string "ERROR HERE"

(* Main function - Run *)
let run ()=
  print_string "=====================\n\tECMA-SL\n=====================\n";
  arguments();
  if (!file = "" && !mode = "" && !out = "") then print_string "No option selected. Use -h"
  else if (!file = "") then  (print_string "No input file. Use -i\n=====================\n\tFINISHED\n=====================\n";exit 1)
  else if(!mode = "") then  (print_string "No mode selected. Use -mode\n=====================\n\tFINISHED\n=====================\n";exit 1);

  compile_from_plus_to_core();


  print_string "=====================\n\tFINISHED\n=====================\n"

let _ = run ()

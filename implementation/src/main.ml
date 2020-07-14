open Parsing_Utils



let file = ref ""
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
			;("-o", Arg.String (fun o -> out := o ), "output file")
			;("-v", Arg.Set verb_aux, "verbose")

	     ]
	     (fun s -> Printf.printf "Ignored Argument: %s" s)
	     usage_msg

(* Main function - Run *)
let run ()=
	print_string "=====================\n\tECMA-SL\n=====================\n";
	arguments();
	if (!file = "" && !mode = "" && !out = "") then print_string "No option selected. Use -h"
	else if (!file = "") then  (print_string "No input file. Use -i\n=====================\n\tFINISHED\n=====================\n";exit 1)
	else if(!mode = "") then  (print_string "No mode selected. Use -mode\n=====================\n\tFINISHED\n=====================\n";exit 1);

	print_string "=====================\n\tFINISHED\n=====================\n"

let _ = run ()
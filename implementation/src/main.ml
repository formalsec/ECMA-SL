let file = ref ""
let heap_file = ref ""
let mode = ref ""
let out = ref ""
let verb_aux = ref false


module NSU = NSU_Monitor.M(SecLevel)
module CoreInterp = Core_Interpreter.M(NSU)
(*
module DCM = Decline_Monitor.M(SecLevel)
module CoreInterp = Core_Interpreter.M(DCM)
*)
(* Argument read function *)


let arguments () =

  let usage_msg = "Usage: -i <path> -mode <c/p> -o <path> -v <bool> -h <path>" in
  Arg.parse
    [
      ("-i", Arg.String (fun f -> file := f), "input file")
    ;("-mode", Arg.String (fun m -> mode := m ), "mode to run: c - Core / p - Plus ")
    ;("-h", Arg.String(fun f -> heap_file := f), "File where to write the computed heap.")
    ;("-o", Arg.String (fun o -> out := o ), "output file")
    ;("-v", Arg.Set verb_aux, "verbose")

    ]
    (fun s -> Printf.printf "Ignored Argument: %s" s)
    usage_msg

let compile_from_plus_to_core () : unit =
  let e_prog_contents = Parsing_Utils.load_file !file in
  let e_prog = Parsing_Utils.parse_e_prog e_prog_contents in
  let e_prog_resolved = Parsing_Utils.resolve_prog_imports e_prog in
  let c_prog = Compiler.compile_prog e_prog_resolved in
  if !out <> "" then Parsing_Utils.write_file (Prog.str c_prog) !out

let core_interpretation () : unit =
  let prog_contents = Parsing_Utils.load_file !file in
  let prog = Parsing_Utils.parse_prog prog_contents in
  print_string "+++++++++++++++++++++++++ JSON +++++++++++++++++++++++++\n";
  print_string (Prog.to_json prog);
  print_string "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  let v, heap = CoreInterp.eval_prog prog !out !verb_aux "main" in
  (match v with
  | Some z -> print_string ("MAIN return -> "^(Val.str z))
  | None -> print_string "ERROR HERE");
  if !heap_file <> ""
  then Parsing_Utils.write_file (Heap.str heap) !heap_file
  else print_endline (Heap.str heap)



(* Main function - Run *)
let run ()=
  print_string "=====================\n\tECMA-SL\n=====================\n";
  arguments();
  if (!file = "" && !mode = "" && !out = "") then print_string "No option selected. Use -h"
  else if (!file = "") then (print_string "No input file. Use -i\n=====================\n\tFINISHED\n=====================\n";exit 1)
  else if (!mode = "") then (print_string "No mode selected. Use -mode\n=====================\n\tFINISHED\n=====================\n";exit 1)
  else if (!mode = "ci") then (print_string "======================= CORE =======================\n"; core_interpretation ())
  else (compile_from_plus_to_core ());


  print_string "\n=====================\n\tFINISHED\n=====================\n"

let _ = run ()

type exit_code =
  | SUCCESS
  | FAILURE
  | ERROR

let exit_prog (code : exit_code) : unit =
  match code with
  | SUCCESS -> exit 0
  | FAILURE -> exit 1
  | ERROR   -> exit 2

let file = ref ""
let heap_file = ref ""
let mode = ref ""
let mon = ref ""
let out = ref ""
let verb_aux = ref false
let silent_aux = ref false
let parse = ref false

let _INLINE_LATTICE_ = "lib/semantics/monitors/nsu_compiler/runtime/H-L-Lattice.esl"

(*(*DEP_LATTICE*)
  module Dep_Lattice = SecLevel_Dep.M
  module NSU = NSU_Monitor.M(Dep_Lattice)

*)

module NSU = NSU_Monitor.M(SecLevel)
module Inliner = NSU_Inliner.M(SecLevel)
module CoreInterp = Core_Interpreter.M(NSU)


(*
module DCM = Decline_Monitor.M(SecLevel)
module CoreInterp = Core_Interpreter.M(DCM)
*)
(* Argument read function *)

let arguments () =

  let usage_msg = "Usage: -i <path> -mode <c/p> -o <path> [-v|-s] -h <path> [--parse]" in
  Arg.parse
    [
      ("-i", Arg.String (fun f -> file := f), "Input file")
    ;("-mode", Arg.String (fun m -> mode := m ), "Mode to run: c - Core / p - Plus ")
    ;("-h", Arg.String(fun f -> heap_file := f), "File where to write the computed heap")
    ;("-o", Arg.String (fun o -> out := o ), "Output file")
    ;("-v", Arg.Set verb_aux, "Verbose")
    ;("-s", Arg.Set silent_aux, "Silent mode")
    ;("-mon", Arg.String(fun m -> mon := m), "Monitor mode")
    ;("--parse", Arg.Set parse, "Parse to JSON")

    ]
    (fun s -> Printf.printf "Ignored Argument: %s" s)
    usage_msg

let combine_progs (prog1 : Prog.t) (prog2 : Prog.t) : Prog.t =
  Hashtbl.iter (fun k v ->
      Prog.add_func prog1 k v) prog2;
  prog1

let parse_program (prog : Prog.t) (inline : string) : unit =
  print_string "+++++++++++++++++++++++++ JSON +++++++++++++++++++++++++\n";
  let json = Prog.to_json prog in
  print_string json;
  print_string "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  let jsonfile = Filename.remove_extension !file  in
  File_Utils.burn_to_disk (jsonfile ^inline^".json") json;
  Printf.printf "%s" jsonfile

let compile_from_plus_to_core () : unit =
  let e_prog_contents = Parsing_Utils.load_file !file in
  let e_prog = Parsing_Utils.parse_e_prog !file e_prog_contents in
  let e_prog_imports_resolved = Parsing_Utils.resolve_prog_imports e_prog in
  let e_prog_macros_applied = Parsing_Utils.apply_prog_macros e_prog_imports_resolved in
  let c_prog = Compiler.compile_prog e_prog_macros_applied in
  if !out <> "" then File_Utils.burn_to_disk !out (Prog.str c_prog)

let inline_compiler () : Prog.t =
  let prog_contents = Parsing_Utils.load_file !file in
  let prog = Parsing_Utils.parse_prog prog_contents in
  let inlined_prog = Inliner.compile_functions prog in
  (* Add Security funcs. H-L-Lattice.esl*)
  let sec_prog_contents = Parsing_Utils.load_file _INLINE_LATTICE_ in
  let lattice_prog = Parsing_Utils.parse_prog sec_prog_contents in
  let final_prog = combine_progs inlined_prog lattice_prog in
  let inlinedfile = Filename.remove_extension !file  in
  File_Utils.burn_to_disk (inlinedfile^"_inlined.esl") (Prog.str final_prog);
  Printf.printf "================= FINAL PROGRAM ================= \n %s \n=================================" (Prog.str final_prog);
  final_prog



let core_interpretation (prog : Prog.t) : exit_code =

  let v, heap = CoreInterp.eval_prog prog (!out, !mon, !verb_aux) "main" in
  if !heap_file <> "" then File_Utils.burn_to_disk !heap_file (Heap.str_with_global heap);
  (match v with
   | Some z -> (match z with
       | Val.Tuple (ret) -> (let completion = List.nth ret 1 in
                             print_string ("MAIN return -> "^ (Val.str (List.nth ret 0))^ "\n");
                             print_string ("MAIN pc -> " ^ (Val.str completion) ^ "\n");
                             match completion with
                             | Val.Tuple c -> if ((List.nth c 1) <> Val.Symbol "normal") then FAILURE else SUCCESS
                             | _           -> SUCCESS
                            )
       | ret -> print_string ("MAIN return -> "^(Val.str z)); SUCCESS
     )
   | None -> print_string "ERROR Core_Interpretation"; ERROR
  )



(* Main function - Run *)
let run () =
  print_string "=====================\n\tECMA-SL\n=====================\n";
  arguments();
  if !silent_aux then Logging.set_silent ();       (* Disable logging (when using "print_endline" and/or "print_string") *)
  let code : exit_code =
  if (!file = "" && !mode = "" && !out = "") then (print_string "No option selected. Use -h"; FAILURE)
  else if (!file = "") then (print_string "No input file. Use -i\n=====================\n\tFINISHED\n=====================\n"; FAILURE)
  else if (!mode = "") then (print_string "No mode selected. Use -mode\n=====================\n\tFINISHED\n=====================\n"; FAILURE)
  else if (!mode = "ci") then (print_string "======================= CORE =======================\n";
                               let prog_contents = Parsing_Utils.load_file !file in
                               let prog = Parsing_Utils.parse_prog prog_contents in
                               if !parse then (parse_program prog "");
                               core_interpretation (prog)
                              )
  else if (!mode = "parse") then (
    let prog_contents = Parsing_Utils.load_file !file in
    let prog = Parsing_Utils.parse_prog prog_contents in
    parse_program prog ""; SUCCESS)
  else if (!mode = "ic") then (print_string "======================= Inlining Monitor =======================\n";
                               let prog = inline_compiler () in
                               (*Run the ci in inlined prog*)
                               if !parse then (parse_program prog "_inlined");
                               core_interpretation (prog)
                              )
  else (compile_from_plus_to_core (); SUCCESS) in

  print_string "\n=====================\n\tFINISHED\n=====================\n";
  exit_prog code


let _ = run ()

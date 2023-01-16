type exit_code = SUCCESS | FAILURE | ERROR | UNSUPPORTED

let exit_prog (code : exit_code) : unit =
  match code with
  | SUCCESS -> exit 0
  | FAILURE -> exit 1
  | ERROR -> exit 2
  | UNSUPPORTED -> exit 3

module NSU = NSU_Monitor.M (SecLevel)
module Inliner = NSU_Inliner.M (SecLevel)
module Interpreter = Interpreter.M (NSU)

let _INLINE_LATTICE_ = "semantics/monitors/nsu_compiler/runtime/H-L-Lattice.esl"

(*
module DCM = Decline_Monitor.M(SecLevel)
module Interpreter = Interpreter.M(DCM)
*)
(* Argument read function *)
let name = "ECMA-SL"
let version = "v0.2"
let banner () = print_endline (name ^ " " ^ version)
let usage = "Usage: " ^ name ^ " [option] [file ...]"

let argspec =
  Arg.align
    [
      ("-i", Arg.String (fun f -> Flags.file := f), " read program from file");
      ("-o", Arg.String (fun o -> Flags.output := o), " write program to file");
      ("-h", Arg.String (fun f -> Flags.heap_file := f), " write heap to file");
      ("-m", Arg.String (fun m -> Flags.mon := m), " monitor mode");
      ( "-mode",
        Arg.String (fun m -> Flags.mode := m),
        " mode to run: c - Core / p - Plus " );
      ( "-v",
        Arg.Unit
          (fun () ->
            banner ();
            exit 0),
        " show version" );
      ( "--workspace",
        Arg.String (fun o -> Flags.workspace := o),
        " workspace directory" );
      ("--verbose", Arg.Set Flags.verbose, " verbose interpreter");
      ("--parse", Arg.Set Flags.parse, " parse to JSON");
    ]

let combine_progs (prog1 : Prog.t) (prog2 : Prog.t) : Prog.t =
  Hashtbl.iter (fun k v -> Prog.add_func prog1 k v) prog2;
  prog1

let parse_program (prog : Prog.t) (inline : string) : unit =
  print_string "+++++++++++++++++++++++++ JSON +++++++++++++++++++++++++\n";
  let json = Prog.to_json prog in
  print_string json;
  print_string "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  let jsonfile = Filename.remove_extension !Flags.file in
  Io.write_file (jsonfile ^ inline ^ ".json") json;
  Printf.printf "%s" jsonfile

let core_of_plus (file : string) : Prog.t =
  let e_prog_contents = Parsing_utils.load_file file in
  let e_prog = Parsing_utils.parse_e_prog file e_prog_contents in
  let e_prog_imports_resolved = Parsing_utils.resolve_prog_imports e_prog in
  let e_prog_macros_applied =
    Parsing_utils.apply_prog_macros e_prog_imports_resolved
  in
  Compiler.compile_prog e_prog_macros_applied

let compile_from_plus_to_core (file : string) : unit =
  let c_prog = core_of_plus file in
  if !Flags.output <> "" then Io.write_file !Flags.output (Prog.str c_prog)
  else print_endline (Prog.str c_prog)

let inline_compiler () : Prog.t =
  let prog_contents = Parsing_utils.load_file !Flags.file in
  let prog = Parsing_utils.parse_prog prog_contents in
  let inlined_prog = Inliner.compile_functions prog in
  (* Add Security funcs. H-L-Lattice.esl*)
  let sec_prog_contents = Parsing_utils.load_file _INLINE_LATTICE_ in
  let lattice_prog = Parsing_utils.parse_prog sec_prog_contents in
  let final_prog = combine_progs inlined_prog lattice_prog in
  let inlinedfile = Filename.remove_extension !Flags.file in
  Io.write_file (inlinedfile ^ "_inlined.esl") (Prog.str final_prog);
  Printf.printf
    "================= FINAL PROGRAM ================= \n\
    \ %s \n\
     ================================="
    (Prog.str final_prog);
  final_prog

let core_interpretation (prog : Prog.t) : exit_code =
  let v, heap = Interpreter.eval_prog prog !Flags.mon !Flags.target in
  if !Flags.heap_file <> "" then
    Io.write_file !Flags.heap_file (Heap.to_string_with_glob heap);
  match v with
  | Some z -> (
      match z with
      | Val.Tuple ret -> (
          let completion = List.nth ret 1 in
          print_string ("MAIN return -> " ^ Val.str (List.nth ret 0) ^ "\n");
          print_string ("MAIN pc -> " ^ Val.str completion ^ "\n");
          match completion with
          | Val.Tuple c ->
              if List.nth c 1 <> Val.Symbol "normal" then FAILURE else SUCCESS
          | _ -> SUCCESS)
      | Val.Str s ->
          let subStr = String.sub s 0 11 in
          if subStr = "Unsupported" then (
            print_string ("MAIN fail -> " ^ subStr);
            UNSUPPORTED)
          else (
            print_string ("MAIN return -> " ^ s);
            SUCCESS)
      | ret ->
          print_string ("MAIN return -> " ^ Val.str z);
          SUCCESS)
  | None ->
      print_string "ERROR Core_Interpretation";
      ERROR

let symbolic_interpretation (prog : Prog.t) : exit_code =
  let testsuite_path = Filename.concat !Flags.workspace "test-suite" in
  Io.safe_mkdir testsuite_path;
  let report = Eval.analyse prog !Flags.target
  and report_file = Filename.concat !Flags.workspace "report.json" in
  Io.write_file report_file (Report.report_to_json report);
  List.iter
    (fun (file, testcase) ->
      let file' = Filename.concat testsuite_path file in
      Io.write_file file' testcase)
    (Report.testsuite_to_json report);
  SUCCESS

(* Main function *)
let () =
  Arg.parse argspec (fun file -> Flags.file := file) usage;
  print_string "=====================\n\tECMA-SL\n=====================\n";
  if !Flags.verbose then Logging.set_verbose ();
  (* Disable logging (when using "print_endline" and/or "print_string") *)
  let code : exit_code =
    if !Flags.file = "" && !Flags.mode = "" && !Flags.output = "" then (
      print_string "No option selected. Use -h";
      FAILURE)
    else if !Flags.file = "" then (
      print_string
        "No input file. Use -i\n\
         =====================\n\
         \tFINISHED\n\
         =====================\n";
      FAILURE)
    else if !Flags.mode = "" then (
      print_string
        "No mode selected. Use -mode\n\
         =====================\n\
         \tFINISHED\n\
         =====================\n";
      FAILURE)
    else if !Flags.mode = "ci" then (
      print_string "======================= CORE =======================\n";
      let prog = Parsing_utils.(parse_prog (load_file !Flags.file)) in
      if !Flags.parse then parse_program prog "";
      core_interpretation prog)
    else if !Flags.mode = "symbolic" then
      let prog =
        if Filename.check_suffix !Flags.file ".esl" then
          core_of_plus !Flags.file
        else if Filename.check_suffix !Flags.file ".cesl" then
          Parsing_utils.(parse_prog (load_file !Flags.file))
        else
          failwith
            ("Unknown file with extension '"
            ^ Filename.extension !Flags.file
            ^ "'")
      in
      symbolic_interpretation prog
    else if !Flags.mode = "parse" then (
      let prog = Parsing_utils.(parse_prog (load_file !Flags.file)) in
      parse_program prog "";
      SUCCESS)
    else if !Flags.mode = "ic" then (
      print_string
        "======================= Inlining Monitor =======================\n";
      let prog = inline_compiler () in
      (*Run the ci in inlined prog*)
      if !Flags.parse then parse_program prog "_inlined";
      core_interpretation prog)
    else (
      compile_from_plus_to_core !Flags.file;
      SUCCESS)
  in
  print_string "\n=====================\n\tFINISHED\n=====================\n";
  exit_prog code

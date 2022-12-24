type exit_code = SUCCESS | FAILURE | ERROR | UNSUPPORTED

let exit_prog (code : exit_code) : unit =
  match code with
  | SUCCESS -> exit 0
  | FAILURE -> exit 1
  | ERROR -> exit 2
  | UNSUPPORTED -> exit 3

module NSU = NSU_Monitor.M (SecLevel)
module Inliner = NSU_Inliner.M (SecLevel)
module CoreInterp = Core_Interpreter.M (NSU)

let _INLINE_LATTICE_ = "semantics/monitors/nsu_compiler/runtime/H-L-Lattice.esl"

(*
module DCM = Decline_Monitor.M(SecLevel)
module CoreInterp = Core_Interpreter.M(DCM)
*)
(* Argument read function *)
let name = "ECMA-SL"
let version = "v0.2"
let banner () = print_endline (name ^ " " ^ version)

let usage = "Usage: " ^ name ^ " [option] [file ...]"

let argspec =
  Arg.align
    [
      ("-i", Arg.String (fun f -> Flags.file := f), " input file");
      ( "-h",
        Arg.String (fun f -> Flags.heap_file := f),
        " file where to write the computed heap" );
      ("-o", Arg.String (fun o -> Flags.output := o), " output file");
      ("-v", Arg.Set Flags.verbose, " verbose");
      ("-s", Arg.Set Flags.silent, " silent mode");
      ("-mon", Arg.String (fun m -> Flags.mon := m), " monitor mode");
      ( "-mode",
        Arg.String (fun m -> Flags.mode := m),
        " mode to run: c - Core / p - Plus " );
      ("--parse", Arg.Set Flags.parse, " parse to JSON");
      ( "--version",
        Arg.Unit
          (fun () ->
            banner ();
            exit 0),
        " show version" );
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
  File_Utils.burn_to_disk (jsonfile ^ inline ^ ".json") json;
  Printf.printf "%s" jsonfile

let compile_from_plus_to_core () : unit =
  let e_prog_contents = Parsing_Utils.load_file !Flags.file in
  let e_prog = Parsing_Utils.parse_e_prog !Flags.file e_prog_contents in
  let e_prog_imports_resolved = Parsing_Utils.resolve_prog_imports e_prog in
  let e_prog_macros_applied =
    Parsing_Utils.apply_prog_macros e_prog_imports_resolved
  in
  let c_prog = Compiler.compile_prog e_prog_macros_applied in
  if !Flags.output <> "" then
    File_Utils.burn_to_disk !Flags.output (Prog.str c_prog)

let inline_compiler () : Prog.t =
  let prog_contents = Parsing_Utils.load_file !Flags.file in
  let prog = Parsing_Utils.parse_prog prog_contents in
  let inlined_prog = Inliner.compile_functions prog in
  (* Add Security funcs. H-L-Lattice.esl*)
  let sec_prog_contents = Parsing_Utils.load_file _INLINE_LATTICE_ in
  let lattice_prog = Parsing_Utils.parse_prog sec_prog_contents in
  let final_prog = combine_progs inlined_prog lattice_prog in
  let inlinedfile = Filename.remove_extension !Flags.file in
  File_Utils.burn_to_disk (inlinedfile ^ "_inlined.esl") (Prog.str final_prog);
  Printf.printf
    "================= FINAL PROGRAM ================= \n\
    \ %s \n\
     ================================="
    (Prog.str final_prog);
  final_prog

let core_interpretation (prog : Prog.t) : exit_code =
  let v, heap =
    CoreInterp.eval_prog prog (!Flags.output, !Flags.mon, !Flags.verbose) "main"
  in
  if !Flags.heap_file <> "" then
    File_Utils.burn_to_disk !Flags.heap_file (Heap.str_with_global heap);
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

(* Main function *)
let () =
  Arg.parse argspec (fun file -> Flags.file := file) usage;
  print_string "=====================\n\tECMA-SL\n=====================\n";
  if !Flags.silent then Logging.set_silent ();
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
      let prog_contents = Parsing_Utils.load_file !Flags.file in
      let prog = Parsing_Utils.parse_prog prog_contents in
      if !Flags.parse then parse_program prog "";
      core_interpretation prog)
    else if !Flags.mode = "parse" then (
      let prog_contents = Parsing_Utils.load_file !Flags.file in
      let prog = Parsing_Utils.parse_prog prog_contents in
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
      compile_from_plus_to_core ();
      SUCCESS)
  in

  print_string "\n=====================\n\tFINISHED\n=====================\n";
  exit_prog code

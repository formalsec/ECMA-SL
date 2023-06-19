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
let name = "ecma-sl"
let version = "v0.2"
let banner () = print_endline (name ^ " " ^ version)
let usage = "Usage: " ^ name ^ " [option] [file ...]"

let argspec =
  Arg.align
    [
      ("-i", Arg.String (fun f -> Config.file := f), " read program from file");
      ("-o", Arg.String (fun o -> Config.output := o), " write program to file");
      ("-h", Arg.String (fun f -> Config.heap_file := f), " write heap to file");
      ("-m", Arg.String (fun m -> Config.mon := m), " monitor mode");
      ( "-mode",
        Arg.String (fun m -> Config.mode := m),
        " mode to run: c - Core / p - Plus " );
      ( "-v",
        Arg.Unit
          (fun () ->
            banner ();
            exit 0),
        " show version" );
      ( "--workspace",
        Arg.String (fun o -> Config.workspace := o),
        " workspace directory" );
      ("--verbose", Arg.Set Config.verbose, " verbose interpreter");
      ("--parse", Arg.Set Config.parse, " parse to JSON");
      ("--untyped", Arg.Set Config.untyped, " disable the type checker");
    ]

let combine_progs (prog1 : Prog.t) (prog2 : Prog.t) : Prog.t =
  Hashtbl.iter (fun k v -> Prog.add_func prog1 k v) prog2;
  prog1

let parse_program (prog : Prog.t) (inline : string) : unit =
  print_string "+++++++++++++++++++++++++ JSON +++++++++++++++++++++++++\n";
  let json = Prog.to_json prog in
  print_string json;
  print_string "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  let jsonfile = Filename.remove_extension !Config.file in
  Io.write_file ~file:(jsonfile ^ inline ^ ".json") ~data:json;
  Printf.printf "%s" jsonfile

let core_of_plus (file : string) : Prog.t =
  let e_prog_contents = Parsing_utils.load_file file in
  let e_prog = Parsing_utils.parse_e_prog file e_prog_contents in
  let e_prog_imports_resolved = Parsing_utils.resolve_prog_imports e_prog in
  let e_prog_macros_applied =
    Parsing_utils.apply_prog_macros e_prog_imports_resolved
  in
  let terrs = T_Checker.type_program e_prog_macros_applied in
  match (!Config.untyped, terrs) with
  | true, _ -> Compiler.compile_prog e_prog_macros_applied
  | false, [] -> Compiler.compile_prog e_prog_macros_applied
  | false, _ ->
      let _ = Printf.eprintf "%s" (T_Checker.terrs_str terrs) in
      exit (-1)

let compile_from_plus_to_core (file : string) : unit =
  let c_prog = core_of_plus file in
  if !Config.output <> "" then
    Io.write_file ~file:!Config.output ~data:(Prog.str c_prog)
  else print_endline (Prog.str c_prog)

let inline_compiler () : Prog.t =
  let prog_contents = Parsing_utils.load_file !Config.file in
  let prog = Parsing_utils.parse_prog prog_contents in
  let inlined_prog = Inliner.compile_functions prog in
  (* Add Security funcs. H-L-Lattice.esl*)
  let sec_prog_contents = Parsing_utils.load_file _INLINE_LATTICE_ in
  let lattice_prog = Parsing_utils.parse_prog sec_prog_contents in
  let final_prog = combine_progs inlined_prog lattice_prog in
  let inlinedfile = Filename.remove_extension !Config.file in
  Io.write_file ~file:(inlinedfile ^ "_inlined.esl") ~data:(Prog.str final_prog);
  Printf.printf
    "================= FINAL PROGRAM ================= \n\
    \ %s \n\
     ================================="
    (Prog.str final_prog);
  final_prog

let core_interpretation (prog : Prog.t) : exit_code =
  let v, heap = Interpreter.eval_prog prog !Config.mon !Config.target in
  if !Config.heap_file <> "" then
    Io.write_file ~file:!Config.heap_file
      ~data:(Heap.to_string_with_glob heap (Val.str ~flt_with_dot:false));
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
  Arg.parse argspec (fun file -> Config.file := file) usage;
  print_string "=====================\n\tECMA-SL\n=====================\n";
  if !Config.verbose then Logging.set_verbose ();
  (* Disable logging (when using "print_endline" and/or "print_string") *)
  let code : exit_code =
    if !Config.file = "" && !Config.mode = "" && !Config.output = "" then (
      print_string "No option selected. Use -h";
      FAILURE)
    else if !Config.file = "" then (
      print_string
        "No input file. Use -i\n\
         =====================\n\
         \tFINISHED\n\
         =====================\n";
      FAILURE)
    else if !Config.mode = "" then (
      print_string
        "No mode selected. Use -mode\n\
         =====================\n\
         \tFINISHED\n\
         =====================\n";
      FAILURE)
    else if !Config.mode = "ci" then (
      print_string "======================= CORE =======================\n";
      let prog = Parsing_utils.(parse_prog (load_file !Config.file)) in
      if !Config.parse then parse_program prog "";
      core_interpretation prog)
    else if !Config.mode = "parse" then (
      let prog = Parsing_utils.(parse_prog (load_file !Config.file)) in
      parse_program prog "";
      SUCCESS)
    else if !Config.mode = "ic" then (
      print_string
        "======================= Inlining Monitor =======================\n";
      let prog = inline_compiler () in
      (*Run the ci in inlined prog*)
      if !Config.parse then parse_program prog "_inlined";
      core_interpretation prog)
    else (
      compile_from_plus_to_core !Config.file;
      SUCCESS)
  in
  print_string "\n=====================\n\tFINISHED\n=====================\n";
  exit_prog code

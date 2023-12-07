type exit_code =
  | SUCCESS
  | FAILURE
  | ERROR
  | UNSUPPORTED

let exit' (code : exit_code) : int =
  match code with SUCCESS -> 0 | FAILURE -> 1 | ERROR -> 2 | UNSUPPORTED -> 3

module NSU = NSU_Monitor.M (SecLevel)
module Inliner = NSU_Inliner.M (SecLevel)
module Interpreter = Interpreter.M (NSU)

let _INLINE_LATTICE_ = "semantics/monitors/nsu_compiler/runtime/H-L-Lattice.esl"

let combine_progs (prog1 : Prog.t) (prog2 : Prog.t) : Prog.t =
  Prog.iteri ~f:(fun ~key ~data -> Prog.add_func prog1 key data) prog2;
  prog1

let parse_program (prog : Prog.t) (inline : string) : unit =
  print_string "+++++++++++++++++++++++++ JSON +++++++++++++++++++++++++\n";
  let json = Prog.to_json prog in
  print_string json;
  print_string "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  let jsonfile = Filename.remove_extension !Config.file in
  Io.write_file ~file:(jsonfile ^ inline ^ ".json") ~data:json;
  Printf.printf "%s" jsonfile

let core_of_plus ~unsafe (file : string) : Prog.t =
  let e_prog_contents = Io.load_file file in
  let e_prog = Parsing_utils.parse_e_prog file e_prog_contents in
  let e_prog_imports_resolved = Parsing_utils.resolve_prog_imports e_prog in
  let e_prog_macros_applied =
    Parsing_utils.apply_prog_macros e_prog_imports_resolved
  in
  let terrs = T_Checker.type_program e_prog_macros_applied in
  match (unsafe, terrs) with
  | true, _ -> Compiler.compile_prog e_prog_macros_applied
  | false, [] -> Compiler.compile_prog e_prog_macros_applied
  | false, _ ->
    let _ = Printf.eprintf "%s" (T_Checker.terrs_str terrs) in
    exit (-1)

let inline_compiler () : Prog.t =
  let prog_contents = Io.load_file !Config.file in
  let prog = Parsing_utils.parse_prog prog_contents in
  let inlined_prog = Inliner.compile_functions prog in
  (* Add Security funcs. H-L-Lattice.esl*)
  let sec_prog_contents = Io.load_file _INLINE_LATTICE_ in
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

let core_interpretation ?heap_file (prog : Prog.t) : exit_code =
  let v, heap = Interpreter.eval_prog prog !Config.mon !Config.target in
  Option.may
    (fun file ->
      Io.write_file ~file
        ~data:(Heap.to_string_with_glob heap (Val.str ~flt_with_dot:false)) )
    heap_file;
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
      | _ -> SUCCESS )
    | Val.Str s ->
      let subStr = String.sub s 0 11 in
      if subStr = "Unsupported" then (
        print_string ("MAIN fail -> " ^ subStr);
        UNSUPPORTED )
      else (
        print_string ("MAIN return -> " ^ s);
        SUCCESS )
    | _ ->
      print_string ("MAIN return -> " ^ Val.str z);
      SUCCESS )
  | None ->
    print_string "ERROR Core_Interpretation";
    ERROR

let compile _debug unsafe (file : string) (output : string option) =
  let c_prog = core_of_plus ~unsafe file in
  ( match output with
  | Some out -> Io.write_file ~file:out ~data:(Prog.str c_prog)
  | None -> Format.printf "%s@." (Prog.str c_prog) );
  0

(* Main function *)
let main _debug file =
  let prog = Parsing_utils.(parse_prog (Io.load_file file)) in
  exit' @@ core_interpretation prog

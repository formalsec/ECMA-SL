let eval_build_ast_func = String_Utils.make_fresh_var_generator "eval_func_"

let parseJS
    (prog  : Prog.t)
    (heap  : Heap.t)
    (str   : string) : Val.t =
  let base = Filename.basename !Flags.file in
  let input_file = "__parse_in_" ^  base ^ "__.js"
  and output_file = "__parse_out_" ^ base ^ "__.js" in
  File_Utils.burn_to_disk input_file str;
  let func_id = eval_build_ast_func () in
  let command = Printf.sprintf "node ../JS2ECMA-SL/src/index.js -c -i %s -o %s -n %s" input_file output_file func_id in
  let _ = Sys.command command in
  let parsed_str = Parsing_Utils.load_file output_file in
  let func = Parsing_Utils.parse_func parsed_str in
  Prog.add_func prog func_id func;
  List.iter Sys.remove [input_file; output_file];
  Val.Str func_id

let loadInitialHeap
    (prog  : Prog.t)
    (heap  : Heap.t)
    (file  : string) : Val.t =
  Val.Loc (Parse_Heap.parse_and_update heap file)


let execute
    (prog  : Prog.t)
    (heap  : Heap.t)
    (f     : string)
    (vs    : Val.t list) : Val.t =
  match f, vs with
  | "parseJS", [ (Val.Str str) ]             -> parseJS prog heap str
  | "loadInitialHeap", [ (Val.Str file) ] -> let loc = loadInitialHeap prog heap file in loc
  | _ -> failwith "UNKNOWN external function"

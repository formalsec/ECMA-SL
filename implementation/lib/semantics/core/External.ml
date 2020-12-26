

let aux_input_file = "__parse_js_aux_input_file__.js"
let aux_output_file = "__parse_js_aux_output_file__.js"

let eval_build_ast_func = String_Utils.make_fresh_var_generator "eval_func_"

let parseJS 
    (prog  : Prog.t)
    (heap  : Heap.t)
    (str   : string) : Val.t = 
  
  File_Utils.burn_to_disk aux_input_file str; 
  let func_id = eval_build_ast_func () in 
  let command = Printf.sprintf "node ../JS2ECMA-SL/src/index.js -i %s -o %s -n %s" aux_input_file aux_output_file func_id in 
  let _ = Sys.command command in 
  let parsed_str = Parsing_Utils.load_file aux_output_file in 
  let efunc = Parsing_Utils.parse_e_func parsed_str in 
  let func = Compiler.compile_func efunc in 
  Prog.add_func prog func_id func; 
  Val.Str func_id

let execute 
    (prog  : Prog.t)
    (heap  : Heap.t)
    (f     : string)
    (vs    : Val.t list) : Val.t = 
  match f, vs with 
  | "parseJS", [ (Val.Str str) ] -> parseJS prog heap str  
  | _ -> failwith "UNKNOWN external function" 

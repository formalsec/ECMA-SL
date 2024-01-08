let eval_build_ast_func = String_utils.make_fresh_var_generator "eval_func_"

let parseJS (prog : Prog.t) (_heap : 'a Heap.t) (str : string) : Val.t =
  let base = Filename.basename !Config.file in
  let input_file = "__parse_in_" ^ base ^ "__.js"
  and output_file = "__parse_out_" ^ base ^ "__.js" in
  Io.write_file ~file:input_file ~data:str;
  let func_id = eval_build_ast_func () in
  let command =
    Printf.sprintf "node ../JS2ECMA-SL/src/index.js -c -i %s -o %s -n %s"
      input_file output_file func_id
  in
  let _ = Sys.command command in
  let parsed_str = Io.load_file output_file in
  let f = Parsing_utils.parse_func parsed_str in
  Prog.add_func prog func_id f;
  List.iter Sys.remove [ input_file; output_file ];
  Val.Str func_id

let loadInitialHeap (_prog : Prog.t) (heap : 'a Heap.t) (file : string) : Val.t
    =
  Val.Loc (Parsing_heap_utils.parse_and_update heap file)

let execute (prog : Prog.t) (heap : 'a Heap.t) (f : string) (vs : Val.t list) :
  Val.t =
  match (f, vs) with
  | ("is_symbolic", _) -> Val.Bool false
  | ("parseJS", [ Val.Str str ]) -> parseJS prog heap str
  | ("loadInitialHeap", [ Val.Str file ]) ->
    let loc = loadInitialHeap prog heap file in
    loc
  | _ -> failwith "UNKNOWN external function"

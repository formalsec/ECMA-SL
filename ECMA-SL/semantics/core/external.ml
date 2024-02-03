let eval_build_ast_func = Utils.make_name_generator "eval_func_"

let parseJS (prog : Prog.t) (_heap : 'a Heap.t) (str : string) : Val.t =
  let _ = failwith "FIXME: Check if we can remove this function" in
  let base = Filename.basename "" in
  let input_file = "__parse_in_" ^ base ^ "__.js"
  and output_file = "__parse_out_" ^ base ^ "__.js" in
  Io.write_file input_file str;
  let func_id = eval_build_ast_func () in
  let command =
    Printf.sprintf "node ../JS2ECMA-SL/src/index.js -c -i %s -o %s -n %s"
      input_file output_file func_id
  in
  let _ = Sys.command command in
  let parsed_str = Parsing_utils.load_file output_file in
  let f = Parsing_utils.parse_func parsed_str in
  Hashtbl.replace prog func_id f;
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
  | _ ->
    Format.eprintf "UNKNOWN %s external function@." f;
    Val.Symbol "undefined"


open EslCore

type store = Val.t Store.t
type heap = Val.t Heap.t

let eval_build_ast_func = Utils.make_name_generator "eval_func_"

let parseJS (prog : Prog.t) (code : string) : Val.t =
  let parse input output builder =
    match Bos.OS.Cmd.run (Js2ecmasl.cmd input (Some output) (Some builder)) with
    | Error _ -> Eslerr.(internal __FUNCTION__ (Custom "error in JS2ECMA-SL"))
    | Ok _ -> ()
  in
  let input = Filename.temp_file "ecmasl" "eval_func.js" in
  let output = Filename.temp_file "ecmasl" "eval_func.cesl" in
  let eval_func_id = eval_build_ast_func () in
  try
    Io.write_file input code;
    parse input output eval_func_id;
    let ast = Io.read_file output in
    let eval_func = Parsing_utils.parse_func ast in
    Hashtbl.replace (Prog.funcs prog) eval_func_id eval_func;
    Val.Str eval_func_id
  with _ -> Eslerr.(internal __FUNCTION__ (Custom "error in ParseJS"))

let execute (prog : Prog.t) (_store : 'a Store.t) (_heap : 'a Heap.t)
  (fn : Id.t') (vs : Val.t list) : Val.t =
  match (fn, vs) with
  | ("is_symbolic", _) -> Val.Bool false
  | ("parseJS", [ Val.Str code ]) -> parseJS prog code
  | _ ->
    Log.warn "UNKNOWN %s external function" fn;
    Val.Symbol "undefined"

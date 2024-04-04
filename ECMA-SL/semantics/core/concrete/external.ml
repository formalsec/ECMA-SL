open EslBase
open EslSyntax

type store = Val.t Store.t
type heap = Val.t Heap.t

let eval_build_ast_func = Base.make_name_generator "eval_func_"

let parseJS (prog : Prog.t) (code : string) : Val.t =
  let input = Filename.temp_file "ecmasl" "eval_func.js" in
  let output = Filename.temp_file "ecmasl" "eval_func.cesl" in
  let eval_func_id = eval_build_ast_func () in
  Io.write_file input code;
  let js2ecmasl = EslJSParser.Api.cmd input (Some output) (Some eval_func_id) in
  match Bos.OS.Cmd.run js2ecmasl with
  | Error _ -> Internal_error.(throw __FUNCTION__ (Custom "err in JS2ECMA-SL"))
  | Ok _ -> (
    try
      let ast_func = Io.read_file output in
      let eval_func = Parsing.parse_func ast_func in
      Hashtbl.replace (Prog.funcs prog) eval_func_id eval_func;
      Val.Str eval_func_id
    with _ -> Internal_error.(throw __FUNCTION__ (Custom "er in ParseJS")) )

let int_to_four_hex (v : Val.t) : Val.t =
  let op_lbl = "int_to_four_hex_external" in
  match v with
  | Int i -> Str (Printf.sprintf "%04x" i)
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "integer" [ v ]


let octal_to_decimal (v : Val.t) : Val.t =
  let op_lbl = "octal_to_decimal_external" in
  match v with
  | Int o ->
    let rec loop dec_value base temp =
      if temp = 0 then dec_value
      else
        let dec_value = dec_value + (temp mod 10 * base) in
        loop dec_value (base * 8) (temp / 10)
    in
    Int (loop 0 1 o)
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "integer" [ v ]

let execute (prog : Prog.t) (_store : 'a Store.t) (_heap : 'a Heap.t)
  (fn : Id.t') (vs : Val.t list) : Val.t =
  match (fn, vs) with
  | ("is_symbolic", _) -> Val.Bool false
  | ("parseJS", [ Val.Str code ]) -> parseJS prog code
  | ("int_to_four_hex_external", [ v ]) -> int_to_four_hex v
  | ("octal_to_decimal_external", [ v ]) -> octal_to_decimal v
  | _ ->
    Log.warn "UNKNOWN %s external function" fn;
    Val.Symbol "undefined"

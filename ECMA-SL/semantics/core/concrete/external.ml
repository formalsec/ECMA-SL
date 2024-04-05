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

let to_precision ((v1, v2) : Val.t * Val.t) : Val.t =
  let op_lbl = "to_precision_external" in
  match (v1, v2) with
  | (Flt x, Int y) ->
    let z = Float.to_int (Float.log10 x) + 1 in
    if y < z then
      let exp = Float.log10 x in
      if exp >= 0. then
        let num =
          Float.round
            (x /. (10. ** Float.trunc exp) *. (10. ** Float.of_int (y - 1)))
          /. (10. ** Float.of_int (y - 1))
        in
        if Float.is_integer num && y = 1 then
          Str
            ( string_of_int (Float.to_int num)
            ^ "e+"
            ^ Int.to_string (Float.to_int exp) )
        else Str (string_of_float num ^ "e+" ^ Int.to_string (Float.to_int exp))
      else
        let num =
          Float.round
            (x /. (10. ** Float.floor exp) *. (10. ** Float.of_int (y - 1)))
          /. (10. ** Float.of_int (y - 1))
        in
        if Float.is_integer num && y = 1 then
          Str
            ( string_of_int (Float.to_int num)
            ^ "e"
            ^ Int.to_string (Float.to_int (Float.floor exp)) )
        else
          Str
            ( string_of_float num
            ^ "e"
            ^ Int.to_string (Float.to_int (Float.floor exp)) )
    else
      let res =
        Float.round (x *. (10. ** float_of_int (y - 1)))
        /. (10. ** float_of_int (y - 1))
      in
      Str (Float.to_string res)
  | (Flt _, _) -> Eval_operator.bad_arg_err 2 op_lbl "(float, integer)" [ v1; v2 ]
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "(float, integer)" [ v1; v2 ]

let to_exponential ((v1, v2) : Val.t * Val.t) : Val.t =
  let op_lbl = "to_exponential_external" in
  match (v1, v2) with
  | (Flt x, Int y) ->
    let exp = Float.log10 x in
    if exp >= 0. then
      let num =
        Float.round (x /. (10. ** Float.trunc exp) *. (10. ** Float.of_int y))
        /. (10. ** Float.of_int y)
      in
      if Float.is_integer num then
        Str
          ( string_of_int (Float.to_int num)
          ^ "e+"
          ^ Int.to_string (Float.to_int exp) )
      else Str (string_of_float num ^ "e+" ^ Int.to_string (Float.to_int exp))
    else
      let num =
        Float.round (x /. (10. ** Float.floor exp) *. (10. ** Float.of_int y))
        /. (10. ** Float.of_int y)
      in
      if Float.is_integer num then
        Str
          ( string_of_int (Float.to_int num)
          ^ "e"
          ^ Int.to_string (Float.to_int (Float.floor exp)) )
      else
        Str
          ( string_of_float num
          ^ "e"
          ^ Int.to_string (Float.to_int (Float.floor exp)) )
  | (Flt _, _) -> Eval_operator.bad_arg_err 2 op_lbl "(float, integer)" [ v1; v2 ]
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "(float, integer)" [ v1; v2 ]
    
let to_fixed ((v1, v2) : Val.t * Val.t) : Val.t =
  let op_lbl = "to_fixed_external" in
  match (v1, v2) with
  | (Flt x, Int y) -> Str (Printf.sprintf "%0.*f" y x)
  | (Flt _, _) -> Eval_operator.bad_arg_err 2 op_lbl "(float, integer)" [ v1; v2 ]
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "(float, integer)" [ v1; v2 ]

let from_char_code_u (v : Val.t) : Val.t =
  let op_lbl = "from_char_code_u_external" in
  match v with
  | Int n -> Str (String_utils.from_char_code_u n)
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "integer" [ v ]
  
let to_char_code_u (v : Val.t) : Val.t =
  let op_lbl = "to_char_code_u_external" in
  match v with
  | Str s -> Int (String_utils.to_char_code_u s)
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "string" [ v ]

let to_lower_case (v : Val.t) : Val.t =
  let op_lbl = "to_lower_case_external" in
  match v with
  | Str s -> Str (String_utils.to_lower_case s)
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "string" [ v ]

let to_upper_case (v : Val.t) : Val.t =
  let op_lbl = "to_upper_case_external" in
  match v with
  | Str s -> Str (String_utils.to_upper_case s)
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "string" [ v ]

let trim (v : Val.t) : Val.t =
  let op_lbl = "trim_external" in
  match v with
  | Str s -> Str (String_utils.trim s)
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "string" [ v ]

let s_len_u (v : Val.t) : Val.t =
  let op_lbl = "s_len_u_external" in
  match v with
  | Str s -> Int (String_utils.s_len_u (s))
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "string" [ v ]

let s_nth_u ((v1, v2) : Val.t * Val.t) : Val.t =
  let op_lbl = "s_nth_u_external" in
  match (v1, v2) with
  | (Str s, Int i) -> (
    try Str (String_utils.s_nth_u s i)
    with _ -> Eval_operator.unexpected_err 2 op_lbl "index out of bounds" )
  | (Str _, _) -> Eval_operator.bad_arg_err 2 op_lbl "(string, integer)" [ v1; v2 ]
  | _ -> Eval_operator.bad_arg_err 1 op_lbl "(string, integer)" [ v1; v2 ]
    
let execute (prog : Prog.t) (_store : 'a Store.t) (_heap : 'a Heap.t)
  (fn : Id.t') (vs : Val.t list) : Val.t =
  match (fn, vs) with
  | ("is_symbolic", _) -> Val.Bool false
  | ("parseJS", [ Val.Str code ]) -> parseJS prog code
  | ("int_to_four_hex_external", [ v ]) -> int_to_four_hex v
  | ("octal_to_decimal_external", [ v ]) -> octal_to_decimal v
  | ("to_precision_external", [ v1 ; v2 ]) -> to_precision (v1, v2)
  | ("to_exponential_external", [ v1 ; v2 ]) -> to_exponential (v1, v2)
  | ("to_fixed_external", [ v1 ; v2 ]) -> to_fixed (v1, v2)
  | ("from_char_code_u_external", [ v ]) -> from_char_code_u v
  | ("to_char_code_u_external", [ v ]) -> to_char_code_u v
  | ("to_lower_case_external", [ v ]) -> to_lower_case v
  | ("to_upper_case_external", [ v ]) -> to_upper_case v
  | ("trim_external", [ v ]) -> trim v
  | ("s_len_u_external", [ v ]) -> s_len_u v
  | ("s_nth_u_external", [ v1 ; v2 ]) -> s_nth_u (v1, v2)
  | _ ->
    Log.warn "UNKNOWN %s external function" fn;
    Val.Symbol "undefined"

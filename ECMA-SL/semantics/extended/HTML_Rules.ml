(* type ctxt_t =
   | BinaryExpr
   | Call
   | IfGuard
   | Set
   | Let
   | Negative
   | Assert
   | AssertNegative
   | ExprStmt
   | Table
   | TopLevel
*)

(* quantas regras com ctxt? *)

type func_call_t = {
  func_name: string;         (* The name of the function *)
  active_params: int list;   (* The indexes of the params that are used to build the resulting phrase *)
  phrases: string list;      (* The phrases that appear before any argument *)
  final_phrase: string;      (* The last phrase. It's optional. In case there's none, an empty string should be used  *)
  (* ctxt: ctxt_t option *)
}

type lookup_t = {
  active_elements: int list;
  phrases: string list;
  prop_name: string;
  final_phrase: string;      (* The last phrase. It's optional. In case there's none, an empty string should be used  *)
}

type binoper_t = {
  oper: string;
  expressions_order: int list;
  phrases: string list;
  final_phrase: string;      (* The last phrase. It's optional. In case there's none, an empty string should be used  *)
}

type t =
  | FuncCall of func_call_t
  | Lookup   of lookup_t
  | BinOper  of binoper_t

let func_call_rules : (string, func_call_t) Hashtbl.t = Hashtbl.create 0
let lookup_rules : (string, lookup_t) Hashtbl.t = Hashtbl.create 0
let binoper_rules : (string, binoper_t) Hashtbl.t = Hashtbl.create 0


let build_phrase (replace : int -> string) (actives : int list) (phrases : string list) (final_phrase : string) : string =
  let strs = List.map replace actives in
  let strs_phrases = List.combine phrases strs in
  let str = String.concat ""
      (List.map (
          (* when one of the strs is the empty string we don't concatenate but instead return the empty string *)
          (* this can happen when one of the function call arguments is the value null *)
          fun (s1, s2) -> if s2 = "" then "" else s1 ^ s2
        ) strs_phrases) in
  str ^ final_phrase


let apply_func_call_rule (rule : func_call_t) (param_to_str : int -> string) : string =
  let param_replace (ix : int) : string =
    if ix = -1 then rule.func_name
    else param_to_str ix in
  build_phrase param_replace rule.active_params rule.phrases rule.final_phrase


let apply_lookup_rule (rule : lookup_t) (obj_str : string) (prop_str : string) : string =
  let element_replace (ix : int) : string =
    if ix = 0 then obj_str
    else if ix = 1 then prop_str
    else raise (Failure "apply_lookup_rule") in
  build_phrase element_replace rule.active_elements rule.phrases rule.final_phrase


let apply_binoper_rule (rule : binoper_t) (oper : string) (left : string) (right : string) : string =
  let replace (ix : int) : string =
    if ix = 0 then left
    else if ix = 1 then right
    else raise (Failure "apply_binoper_rule") in
  build_phrase replace rule.expressions_order rule.phrases rule.final_phrase


let apply_func_call_rule_by_name (func_name : string) (param_to_str : int -> string) : string option =
  let rule_opt = Hashtbl.find_opt func_call_rules func_name in
  match rule_opt with
  | None -> None
  | Some rule -> Some (apply_func_call_rule rule param_to_str)


let apply_lookup_rule_by_name (prop_name : string) (obj_to_str : unit -> string) : string option =
  let rule_opt = Hashtbl.find_opt lookup_rules prop_name in
  match rule_opt with
  | None -> None
  | Some rule -> Some (apply_lookup_rule rule (obj_to_str ()) prop_name)


let apply_oper_rule_by_name (oper_name : string) (expr_to_str : int -> string) : string option =
  let rule_opt = Hashtbl.find_opt binoper_rules oper_name in
  match rule_opt with
  | None -> None
  | Some rule -> Some (apply_binoper_rule rule oper_name (expr_to_str 0) (expr_to_str 1))


let create_func_call_t (func_call_obj : Yojson.Basic.t) : func_call_t list =
  let open Yojson.Basic.Util in
  try
    let func_names =
      match func_call_obj |> member "func_name" with
      | `List l   -> l |> filter_string
      | `String s -> [s]
      | _ -> invalid_arg "Error creating func_call_t object; invalid \"func_name\" value: expecting JSON array or string."
    in
    List.map (
      fun fn ->
        {
          func_name     = fn;
          active_params = func_call_obj |> member "active_params" |> to_list |> filter_int;
          phrases       = func_call_obj |> member "phrases" |> to_list |> filter_string;
          final_phrase  = Option.default "" (func_call_obj |> member "final_phrase" |> to_string_option);
        }
    ) func_names
  with Type_error (exn_str, _) ->
    invalid_arg (
      "Error creating func_call_t object; missing required property in the JSON object: " ^
      exn_str
    )


let create_lookup_t (lookup_obj : Yojson.Basic.t) : lookup_t list =
  let open Yojson.Basic.Util in
  try
    let prop_names =
      match lookup_obj |> member "prop_name" with
      | `List l   -> l |> filter_string
      | `String s -> [s]
      | _ -> invalid_arg "Error creating lookup_t object; invalid \"prop_name\" value: expecting JSON array or string."
    in
    List.map (
      fun pn ->
        {
          prop_name       = pn;
          active_elements = lookup_obj |> member "active_elements" |> to_list |> filter_int;
          phrases         = lookup_obj |> member "phrases" |> to_list |> filter_string;
          final_phrase    = Option.default "" (lookup_obj |> member "final_phrase" |> to_string_option);
        }
    ) prop_names
  with Type_error (exn_str, _) ->
    invalid_arg (
      "Error creating lookup_t object; missing required property in the JSON object: " ^
      exn_str
    )


let create_binoper_t (binoper_obj : Yojson.Basic.t) : binoper_t list =
  let open Yojson.Basic.Util in
  try
    let opers =
      match binoper_obj |> member "oper" with
      | `List l   -> l |> filter_string
      | `String s -> [s]
      | _ -> invalid_arg "Error creating binoper_t object; invalid \"oper\" value: expecting JSON array or string."
    in
    List.map (
      fun op ->
        {
          oper              = op;
          expressions_order = binoper_obj |> member "expressions_order" |> to_list |> filter_int;
          phrases           = binoper_obj |> member "phrases" |> to_list |> filter_string;
          final_phrase      = Option.default "" (binoper_obj |> member "final_phrase" |> to_string_option);
        }
    ) opers
  with Type_error (exn_str, _) ->
    invalid_arg (
      "Error creating binoper_t object; missing required property in the JSON object: " ^
      exn_str
    )


let parse_rules (rules_obj : Yojson.Basic.t) (map_f : Yojson.Basic.t -> t list) : t list =
  match rules_obj with
  | `List rules -> List.flatten (List.map map_f rules)
  | _ -> invalid_arg "Error parsing HTML rules; unexpected type: expecting List"


let update_tables (rules : t list) =
  List.iter (
    fun (rule : t) ->
      match rule with
      | FuncCall fc ->
        Hashtbl.add func_call_rules fc.func_name fc
      | Lookup l ->
        Hashtbl.add lookup_rules l.prop_name l
      | BinOper bop ->
        Hashtbl.add binoper_rules bop.oper bop
  ) rules


let parse_json (json : Yojson.Basic.t) : t list =
  match json with
  (*
  [
    {
      type: "",
      rules: [
        { ... }
      ]
    }, ...
  ]
  *)
  | `List type_rules_assocs ->
    let f_call_ts =
      List.map (
        fun type_rules_assoc ->
          match type_rules_assoc with
          | `Assoc type_rules ->
            (match type_rules with
             | [("type", `String "func_call_rules"); ("rules", func_call_rules)] ->
               parse_rules
                 func_call_rules
                 (fun r ->
                    match r with
                    | `Assoc k_v ->
                      List.map (fun fc -> FuncCall fc) (create_func_call_t r)
                    | _          ->
                      invalid_arg "Error parsing HTML rules; unexpected JSON object: cannot convert to \"FuncCall\" type"
                 )
             | [("type", `String "lookup_rules"); ("rules", lookup_rules)] ->
               parse_rules
                 lookup_rules
                 (fun r ->
                    match r with
                    | `Assoc k_v ->
                      List.map (fun l -> Lookup l) (create_lookup_t r)
                    | _ ->
                      invalid_arg "Error parsing HTML rules; unexpected JSON object: cannot convert to \"Lookup\" type"
                 )
             | [("type", `String "binop_rules"); ("rules", binop_rules)] ->
               parse_rules
                 binop_rules
                 (fun r ->
                    match r with
                    | `Assoc k_v ->
                      List.map (fun bop -> BinOper bop) (create_binoper_t r)
                    | _ ->
                      invalid_arg "Error parsing HTML rules; unexpected JSON object: cannot convert to \"BinOper\" type"
                 )
             | _ -> invalid_arg "Error parsing HTML rules: unknown JSON object")
          | _ -> invalid_arg "Unexpected HTML rules JSON file; Expecting JSON object."
      ) type_rules_assocs in
    List.flatten f_call_ts
  | _ -> invalid_arg "Unexpected HTML rules JSON file; Expecting a list as the first element."


let load_json_file (js_file : string) : Yojson.Basic.t =
  let data = Yojson.Basic.from_file js_file in
  data

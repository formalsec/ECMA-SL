open T_Err

let syntax_test (file : string) : unit =
  let data = Parsing_utils.load_file file in
  ignore (Parsing_utils.parse_e_prog file data)

let type_checker_succ (file : string) : bool =
  let data = Parsing_utils.load_file file in
  let prog = Parsing_utils.parse_e_prog file data in
  let terrors = T_Checker.type_program prog in
  terrors == []

let type_checker_fail (file : string) (eerrors : T_Err.err list) : bool =
  let data = Parsing_utils.load_file file in
  let prog = Parsing_utils.parse_e_prog file data in
  let terrors = List.map (fun terr -> terr.err) (T_Checker.type_program prog) in
  List.for_all (fun (terr, eerr) -> terr = eerr) (List.combine terrors eerrors)

let type_checker_comp (file : string) : unit = ignore (type_checker_succ file)

(* Auxiliary Functions *)

let obj_fun (ps : (string * E_Type.t) list) : E_Type.t =
  let obj_field_fun (fn, ft) = (fn, { E_Type.t = ft; E_Type.opt = false }) in
  let flds = Hashtbl.of_seq (List.to_seq (List.map obj_field_fun ps)) in
  E_Type.ObjectType { flds; E_Type.smry = None }

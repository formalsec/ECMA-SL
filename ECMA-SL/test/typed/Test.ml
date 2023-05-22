open T_Err

let type_checker_test (file : string) (eerrors : err_t list) : bool =
  let data = Parsing_utils.load_file file in
  let prog = Parsing_utils.parse_e_prog file data in
  let main_terr_fun terr = List.nth terr.errs (List.length terr.errs - 1) in
  let terrors = List.map main_terr_fun (T_Checker.type_program prog) in
  if List.length terrors != List.length eerrors then false
  else
    List.for_all
      (fun (terr, eerr) -> terr = eerr)
      (List.combine terrors eerrors)

let obj_cons (ps : (string * E_Type.t) list) : E_Type.t =
  let obj_tfld_fun ft = { E_Type.t = ft; E_Type.status = E_Type.Required } in
  let obj_fld_fun (fn, ft) = (fn, obj_tfld_fun ft) in
  let flds = Hashtbl.of_seq (List.to_seq (List.map obj_fld_fun ps)) in
  E_Type.ObjectType { flds; E_Type.smry = None }

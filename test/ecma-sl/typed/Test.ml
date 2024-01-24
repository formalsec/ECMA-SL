open Ecma_sl
open T_Err

let type_checker_test (file : string) (eerrs : err_t list) : bool =
  let data = Io.read_file file in
  let prog = Parsing_utils.parse_e_prog file data in
  let _main_terr_f terr = List.nth terr.errs (List.length terr.errs - 1) in
  let terrs = List.map _main_terr_f (T_Checker.type_program prog) in
  if List.length terrs != List.length eerrs then false
  else List.for_all (fun (terr, eerr) -> terr = eerr) (List.combine terrs eerrs)

let obj_cons (flds : (string * EType.t) list) : EType.t =
  let _obj_fld_f (fn, ft) = (fn, (ft, EType.Required)) in
  let flds = Hashtbl.of_seq (List.to_seq (List.map _obj_fld_f flds)) in
  EType.ObjectType { flds; EType.smry = None }

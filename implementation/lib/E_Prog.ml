type t = (string, E_Func.t) Hashtbl.t

let create (funcs : E_Func.t list) : t =
  let prog = Hashtbl.create 511 in
  List.iter (fun (f : E_Func.t) -> Hashtbl.add prog f.name f) funcs;
  match Hashtbl.find_opt prog "main" with
    None   -> invalid_arg "Missing main function"
  | Some _ -> prog

let get_func (prog : t) (func : string) : E_Func.t = Hashtbl.find prog func

let str (prog : t) : string = Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ "\n" else ac) ^ (Printf.sprintf "(%s -> %s)" n (E_Func.str v))) prog ""

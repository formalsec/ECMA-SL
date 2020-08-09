type t = {
  imports : string list;
  funcs   : (string, E_Func.t) Hashtbl.t
}

let create (imports : string list) (funcs : E_Func.t list) : t =
  let prog = {
    imports;
    funcs   = Hashtbl.create 511
  } in
  List.iter (fun (f : E_Func.t) -> Hashtbl.add prog.funcs f.name f) funcs;
  match Hashtbl.find_opt prog.funcs "main" with
    None   -> invalid_arg "Missing main function"
  | Some _ -> prog

let get_func (prog : t) (func : string) : E_Func.t = Hashtbl.find prog.funcs func

let str (prog : t) : string =
  (String.concat " " (List.map (fun i -> "import " ^ i) prog.imports)) ^
  Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ "\n" else ac) ^ (Printf.sprintf "(%s -> %s)" n (E_Func.str v))) prog.funcs ""

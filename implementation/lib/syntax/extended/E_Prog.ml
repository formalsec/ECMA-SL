type t = {
  imports : string list;
  funcs   : (string, E_Func.t) Hashtbl.t
}

let add_funcs (prog : t) (funcs : E_Func.t list) : unit =
  List.iter (fun (f : E_Func.t) -> match Hashtbl.find_opt prog.funcs f.name with
      | None   -> Hashtbl.add prog.funcs f.name f
      | Some _ -> invalid_arg ("Function \"" ^ f.name ^ "\" already exists in the program")
    ) funcs


let create (imports : string list) (funcs : E_Func.t list) : t =
  let prog = {
    imports;
    funcs   = Hashtbl.create 511
  } in
  add_funcs prog funcs;
  prog

let get_func (prog : t) (func : string) : E_Func.t = Hashtbl.find prog.funcs func

let str (prog : t) : string =
  (String.concat " " (List.map (fun i -> "import " ^ i) prog.imports)) ^
  Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ "\n" else ac) ^ (Printf.sprintf "(%s -> %s)" n (E_Func.str v))) prog.funcs ""

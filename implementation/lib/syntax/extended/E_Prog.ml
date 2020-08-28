type t = {
  mutable file_name : string;
  imports           : string list;
  funcs             : (string, E_Func.t) Hashtbl.t
}

let add_funcs (prog : t) (funcs : E_Func.t list) : unit =
  List.iter (fun (f : E_Func.t) -> match Hashtbl.find_opt prog.funcs f.name with
      | None   -> Hashtbl.replace prog.funcs f.name f
      | Some _ -> invalid_arg ("Function \"" ^ f.name ^ "\" already exists in the program")
    ) funcs

let create (imports : string list) (funcs : E_Func.t list) : t =
  let prog = {
    file_name = "temporary name";
    imports;
    funcs = Hashtbl.create 511
  } in
  add_funcs prog funcs;
  prog

let get_file_name (prog : t) : string = prog.file_name

let get_func (prog : t) (func : string) : E_Func.t = Hashtbl.find prog.funcs func

let get_funcs (prog : t) : E_Func.t list = List.of_seq (Hashtbl.to_seq_values prog.funcs)

let get_imports (prog : t) : string list = prog.imports

let set_file_name (prog : t) (file_name : string) : unit = prog.file_name <- file_name

let str (prog : t) : string =
  prog.file_name ^ "\n" ^
  (String.concat " " (List.map (fun i -> "import " ^ i) prog.imports)) ^
  Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ "\n" else ac) ^ (Printf.sprintf "(%s -> %s)" n (E_Func.str v))) prog.funcs ""

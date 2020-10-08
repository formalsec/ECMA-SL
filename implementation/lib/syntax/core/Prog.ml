type t = (string, Func.t) Hashtbl.t

let create (funcs : Func.t list) : t =
  let prog = Hashtbl.create 511 in
  List.iter (fun (f : Func.t) -> Hashtbl.replace prog f.name f) funcs;
  match Hashtbl.find_opt prog "main" with
    None   -> invalid_arg "Missing main function"
  | Some _ -> prog


let get_func (prog : t ) (id : string) : Func.t =
  try
    Hashtbl.find prog id
  with _ ->
    (Printf.printf "Could not find function %s" id;
    raise (Failure "Function not found."))

let get_body (prog: t ) (id: string) : Stmt.t=
  let s = get_func prog id in
  s.body


let get_params (prog: t ) (id: string) : string list =
  let s = get_func prog id in
  s.params

let get_name (prog: t ) (id: string) : string  =
  let s = get_func prog id in
  s.name

let add_func (prog:t) (k: string) (v : Func.t): unit =
  Hashtbl.replace prog k v

let get_funcs (prog : t) : Func.t list = 
  Hashtbl.fold 
    (fun _ f fs -> f::fs)
    prog 
    []   


(*------------Strings----------*)

let str (prog : t) : string =
  String.concat ";\n"  (List.map Func.str (get_funcs prog))

let to_json (prog : t) : string =
  Printf.sprintf "{\"type\" : \" prog\", \"funcs\" : [ %s ] }"  (String.concat ", "  (List.map (Func.to_json) (get_funcs prog)))

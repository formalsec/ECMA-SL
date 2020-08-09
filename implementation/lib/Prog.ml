type t = {
  imports : string list;
  funcs   : (string, Func.t) Hashtbl.t
}

let create (imports : string list) (funcs : Func.t list) : t =
  let prog = {
    imports;
    funcs   = Hashtbl.create 511
  } in
  List.iter (fun (f : Func.t) -> Hashtbl.add prog.funcs f.name f) funcs;
  match Hashtbl.find_opt prog.funcs "main" with
    None   -> invalid_arg "Missing main function"
  | Some _ -> prog


let get_func (prog : t ) (id : string) : Func.t =
  Hashtbl.find prog.funcs id

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
  Hashtbl.replace prog.funcs k v

(*------------Strings----------*)

let str (prog : t) : string =
  (String.concat "" (List.map (fun i -> "import " ^ i ^ ";\n") prog.imports)) ^
  Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ "\n" else ac) ^ (Printf.sprintf "(%s -> %s)" n (Func.str v))) prog.funcs ""

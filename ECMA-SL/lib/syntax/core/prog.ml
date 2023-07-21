open Core
open Func

let ( let+ ) o f = Result.map o ~f

type t = (String.t, Func.t) Caml.Hashtbl.t

let create_empty () : t = Caml.Hashtbl.create !Config.default_hashtbl_sz

let create (funcs : Func.t list) : t =
  let prog = Caml.Hashtbl.create !Config.default_hashtbl_sz in
  List.iter ~f:(fun (f : Func.t) -> Caml.Hashtbl.replace prog f.name f) funcs;
  prog

let get_func (prog : t) (id : string) : (Func.t, string) Result.t =
  let f = Stdlib.Hashtbl.find_opt prog id in
  Result.of_option f ~error:(Format.sprintf "Could not find function %s " id)

let get_body (prog : t) (id : string) : (Stmt.t, string) Result.t =
  let+ s = get_func prog id in
  s.body

let get_params (prog : t) (id : string) : (string list, string) Result.t =
  let+ s = get_func prog id in
  s.params

let get_name (prog : t) (id : string) : (string, string) Result.t =
  let+ s = get_func prog id in
  s.name

let add_func (prog : t) (k : string) (v : Func.t) : unit =
  Caml.Hashtbl.replace prog k v

let get_funcs (prog : t) : Func.t list =
  Caml.Hashtbl.fold (fun _ f fs -> f :: fs) prog []

(*------------Strings----------*)

let str (prog : t) : string =
  String.concat ~sep:";\n" (List.map ~f:Func.str (get_funcs prog))

let to_json (prog : t) : string =
  Printf.sprintf "{\"type\" : \" prog\", \"funcs\" : [ %s ] }"
    (String.concat ~sep:", " (List.map ~f:Func.to_json (get_funcs prog)))

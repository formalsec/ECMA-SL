open Core
open Func

let ( let+ ) o f = Result.map o ~f


type t = (string, Func.t) Hashtbl.t

let empty () : t = Hashtbl.create (module String)

let create (funcs : Func.t list) : t =
  let env = empty () in
  List.iter funcs ~f:(fun f -> Hashtbl.set env ~key:f.name ~data:f);
  env

let get_func (prog : t) (id : string) : (Func.t, string) Result.t =
  let f = Hashtbl.find prog id in
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

let add_func (prog : t) (key : string) (data : Func.t) : unit =
  Hashtbl.set prog ~key ~data

let get_funcs (prog : t) : Func.t list = Hashtbl.data prog [@@inline]

let iter (prog : t) ~(f : (Func.t -> unit)) : unit =
  Hashtbl.iter prog ~f

let iteri (prog : t) ~(f : (key:string -> data:Func.t -> unit)) : unit =
  Hashtbl.iteri prog ~f

let str (prog : t) : string =
  String.concat ~sep:";\n" (List.map ~f:Func.str (get_funcs prog))

let to_json (prog : t) : string =
  Printf.sprintf "{\"type\" : \" prog\", \"funcs\" : [ %s ] }"
    (String.concat ~sep:", " (List.map ~f:Func.to_json (get_funcs prog)))

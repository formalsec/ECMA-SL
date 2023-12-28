open Func

let ( let+ ) o f = Result.map f o

type t = (string, Func.t) Hashtbl.t

let empty () : t = Hashtbl.create !Config.default_hashtbl_sz

let create (funcs : Func.t list) : t =
  let env = empty () in
  List.iter (fun f -> Hashtbl.replace env f.name f) funcs;
  env

let funcs (prog : t) : Func.t list =
  let _func_acc_f _ func acc = func :: acc in
  Hashtbl.fold _func_acc_f prog []

let func (prog : t) (fname : string) : (Func.t, string) Result.t =
  match Hashtbl.find_opt prog fname with
  | None -> Result.error (Printf.sprintf "Cannot find function '%s'." fname)
  | Some f -> Result.ok f

let func_name (prog : t) (fname : string) : (string, string) Result.t =
  let+ s = func prog fname in
  s.name

let func_body (prog : t) (fname : string) : (Stmt.t, string) Result.t =
  let+ s = func prog fname in
  s.body

let func_params (prog : t) (fname : string) : (string list, string) Result.t =
  let+ s = func prog fname in
  s.params

let add_func (prog : t) (fname : string) (func : Func.t) : unit =
  Hashtbl.replace prog fname func

let str (prog : t) : string =
  List.map Func.str (funcs prog) |> String.concat ";\n"

let to_json (prog : t) : string =
  let funcs_json = List.map Func.to_json (funcs prog) |> String.concat ", " in
  Printf.sprintf "{ \"type\" : \"prog\", \"funcs\" : [ %s ] }" funcs_json

open Func

let ( let+ ) o f = Result.map f o

type t = (string, Func.t) Hashtbl.t

let empty () : t = Hashtbl.create !Config.default_hashtbl_sz

let create (funcs : Func.t list) : t =
  let env = empty () in
  List.iter (fun f -> Hashtbl.replace env f.name f) funcs;
  env

let func_opt (prog : t) (fn : string) : Func.t option = Hashtbl.find_opt prog fn

let func (prog : t) (fn : string) : (Func.t, string) Result.t =
  match func_opt prog fn with
  | Some f -> Result.ok f
  | None -> Result.error (Printf.sprintf "Cannot find function '%s'." fn)

let func_name (prog : t) (fn : string) : (string, string) Result.t =
  let+ s = func prog fn in
  s.name

let func_body (prog : t) (fn : string) : (Stmt.t, string) Result.t =
  let+ s = func prog fn in
  s.body

let func_params (prog : t) (fn : string) : (string list, string) Result.t =
  let+ s = func prog fn in
  s.params

let funcs (prog : t) : Func.t list =
  let _func_acc_f _ func acc = func :: acc in
  Hashtbl.fold _func_acc_f prog []

let add_func (prog : t) (fn : string) (func : Func.t) : unit =
  Hashtbl.replace prog fn func

let str (prog : t) : string =
  List.map Func.str (funcs prog) |> String.concat ";\n"

let to_json (prog : t) : string =
  let funcs_json = List.map Func.to_json (funcs prog) |> String.concat ", " in
  Printf.sprintf "{ \"type\" : \"prog\", \"funcs\" : [ %s ] }" funcs_json

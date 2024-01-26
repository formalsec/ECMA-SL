type t = (string, Func.t) Hashtbl.t

let default () : t = Hashtbl.create !Config.default_hashtbl_sz

let create (fs : Func.t list) : t =
  let p = default () in
  List.iter (fun f -> Hashtbl.replace p (Func.name' f) f) fs;
  p

let funcs (p : t) : (string, Func.t) Hashtbl.t = p
let func_opt (p : t) (fn : string) : Func.t option = Hashtbl.find_opt p fn

let func (p : t) (fn : string) : (Func.t, string) Result.t =
  match func_opt p fn with
  | None -> Result.error (Printf.sprintf "Cannot find function '%s'." fn)
  | Some f -> Result.ok f

let func_name (p : t) (fn : string) : (string, string) Result.t =
  Result.map Func.name' (func p fn)

let pp (fmt : Fmt.t) (p : t) : unit =
  Fmt.(fprintf fmt "%a" (pp_hashtbl ";\n" (fun fmt (_, f) -> Func.pp fmt f)) p)

let str (p : t) : string = Fmt.asprintf "%a" pp p

open Source
open Func

type t = (string, Func.t) Hashtbl.t

let default () : t = Hashtbl.create !Config.default_hashtbl_sz

let create (fs : Func.t list) : t =
  let p = default () in
  List.iter (fun f -> Hashtbl.replace p f.it.name.it f) fs;
  p

let func_opt (p : t) (fn : string) : Func.t option = Hashtbl.find_opt p fn

let func (p : t) (fn : string) : (Func.t, string) Result.t =
  match func_opt p fn with
  | Some f -> Result.ok f
  | None -> Result.error (Printf.sprintf "Cannot find function '%s'." fn)

let func_name (p : t) (fn : string) : (string, string) Result.t =
  Result.map (fun f -> f.it.name.it) (func p fn)

let add_func (p : t) (fn : string) (f : Func.t) : unit = Hashtbl.replace p fn f

let pp (fmt : Fmt.t) (p : t) : unit =
  Fmt.(fprintf fmt "%a" (pp_hashtbl ";\n" (fun fmt (_, v) -> Func.pp fmt v)) p)

let str (p : t) : string = Fmt.asprintf "%a" pp p

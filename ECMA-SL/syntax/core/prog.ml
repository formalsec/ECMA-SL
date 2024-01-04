open Source
open Func

type t = (string, Func.t) Hashtbl.t

let empty () : t = Hashtbl.create !Config.default_hashtbl_sz

let create (funcs : Func.t list) : t =
  let env = empty () in
  List.iter (fun f -> Hashtbl.replace env f.it.name f) funcs;
  env

let func_opt (prog : t) (fn : string) : Func.t option = Hashtbl.find_opt prog fn

let func (prog : t) (fn : string) : (Func.t, string) Result.t =
  match func_opt prog fn with
  | Some f -> Result.ok f
  | None -> Result.error (Printf.sprintf "Cannot find function '%s'." fn)

let func_name (prog : t) (fn : string) : (string, string) Result.t =
  Result.map (fun f -> f.it.name) (func prog fn)

let add_func (prog : t) (fn : string) (func : Func.t) : unit =
  Hashtbl.replace prog fn func

let pp (fmt : Format.formatter) (prog : t) : unit =
  let open Format in
  let pp_sep sep fmt () = pp_print_string fmt sep in
  let pp_seq sep pp fmt lst = pp_print_seq ~pp_sep:(pp_sep sep) pp fmt lst in
  fprintf fmt "%a" (pp_seq ";\n" Func.pp) (Hashtbl.to_seq_values prog)

let str (prog : t) : string = Format.asprintf "%a" pp prog

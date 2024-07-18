open EslBase

type t = (Id.t', Func.t) Hashtbl.t

let default () : t = Hashtbl.create !Base.default_hashtbl_sz

let create (funcs : Func.t list) : t =
  let prog = default () in
  List.iter (fun f -> Hashtbl.replace prog (Func.name' f) f) funcs;
  prog

let funcs (prog : t) : (Id.t', Func.t) Hashtbl.t = prog [@@inline]
let func_opt (prog : t) (fn : Id.t') : Func.t option = Hashtbl.find_opt prog fn

let func (prog : t) (fn : Id.t') : (Func.t, string) Result.t =
  match func_opt prog fn with
  | Some f -> Result.ok f
  | None -> Result.error (Fmt.str "Cannot find function '%s'." fn)

let add_func (prog : t) (fn : Id.t') (func : Func.t) : unit =
  Hashtbl.replace prog fn func

let pp (ppf : Fmt.t) (prog : t) : unit =
  let pp_func ppf (_, f) = Func.pp ppf f in
  Fmt.(pp_hashtbl !>";@\n" pp_func) ppf prog

let str (prog : t) : string = Fmt.str "%a" pp prog [@@inline]

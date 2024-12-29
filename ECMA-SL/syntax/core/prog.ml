open EslBase

type t = (Id.t', Func.t) Hashtbl.t

let default () : t = Hashtbl.create !Base.default_hashtbl_sz

let create (fs : Func.t list) : t =
  let p = default () in
  List.iter (fun f -> Hashtbl.replace p (Func.name' f) f) fs;
  p

let funcs (p : t) : (Id.t', Func.t) Hashtbl.t = p [@@inline]
let func (p : t) (fn : Id.t') : Func.t option = Hashtbl.find_opt p fn [@@inline]

let add_func (p : t) (fn : Id.t') (f : Func.t) : unit = Hashtbl.replace p fn f
[@@inline]

let pp (ppf : Format.formatter) (p : t) : unit =
  let pp_func ppf (_, f) = Func.pp ppf f in
  let sep ppf () = Fmt.pf ppf ";@\n" in
  Fmt.(hashtbl ~sep pp_func) ppf p

let str (p : t) : string = Fmt.str "%a" pp p [@@inline]

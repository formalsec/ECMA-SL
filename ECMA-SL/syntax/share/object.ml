open EslBase

type 'a t = (string, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create !Base.default_hashtbl_sz
let clone (obj : 'a t) = Hashtbl.copy obj
let get (obj : 'a t) (fn : string) : 'a option = Hashtbl.find_opt obj fn
let set (obj : 'a t) (fn : string) (fv : 'a) : unit = Hashtbl.replace obj fn fv
let delete (obj : 'a t) (fn : string) : unit = Hashtbl.remove obj fn

let fld_lst (obj : 'a t) : (string * 'a) list =
  let fld_lst_f fn fv acc = (fn, fv) :: acc in
  Hashtbl.fold fld_lst_f obj []

let pp (pp_v : 'a Fmt.t) (ppf : Format.formatter) (obj : 'a t) : unit =
  let pp_fld ppf (fn, fv) = Fmt.pf ppf "%s: %a" fn pp_v fv in
  if Hashtbl.length obj = 0 then Fmt.string ppf "{}"
  else Fmt.(pf ppf "{ %a }" (Fmt.hashtbl ~sep:sp pp_fld) obj)

let str (pp_v : 'a Fmt.t) (obj : 'a t) : string = Fmt.str "%a" (pp pp_v) obj
[@@inline]

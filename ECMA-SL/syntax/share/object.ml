open EslCore

type 'a t = (string, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create !Base.default_hashtbl_sz
let clone (obj : 'a t) = Hashtbl.copy obj

let fld_lst (obj : 'a t) : (string * 'a) list =
  let fld_lst_f fn fv acc = (fn, fv) :: acc in
  Hashtbl.fold fld_lst_f obj []

let flds (obj : 'a t) : string list =
  let fld_name_lst_f fn _ acc = fn :: acc in
  Hashtbl.fold fld_name_lst_f obj []

let get (obj : 'a t) (fn : string) : 'a option = Hashtbl.find_opt obj fn
let set (obj : 'a t) (fn : string) (fv : 'a) : unit = Hashtbl.replace obj fn fv
let delete (obj : 'a t) (fn : string) : unit = Hashtbl.remove obj fn

let pp (pp_val : Fmt.t -> 'a -> unit) (fmt : Fmt.t) (obj : 'a t) : unit =
  let open Fmt in
  let pp_fld fmt (fn, fv) = fprintf fmt "%S: %a" fn pp_val fv in
  if Hashtbl.length obj = 0 then pp_str fmt "{}"
  else fprintf fmt "{ %a }" (pp_hashtbl ", " pp_fld) obj

let str (pp_val : Fmt.t -> 'a -> unit) (obj : 'a t) : string =
  Fmt.asprintf "%a" (pp pp_val) obj

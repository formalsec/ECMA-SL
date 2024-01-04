type 'a t = (string, 'a) Hashtbl.t
type 'a pp_fmt = Format.formatter -> 'a -> unit

let create () : 'a t = Hashtbl.create !Config.default_hashtbl_sz
let clone (obj : 'a t) = Hashtbl.copy obj

let fld_list (obj : 'a t) : (string * 'a) list =
  let _to_fld_lst_f fn fv acc = (fn, fv) :: acc in
  Hashtbl.fold _to_fld_lst_f obj []

let flds (obj : 'a t) : string list =
  let _fld_names_lst_f fn _ acc = fn :: acc in
  Hashtbl.fold _fld_names_lst_f obj []

let get (obj : 'a t) (fn : string) : 'a option = Hashtbl.find_opt obj fn
let set (obj : 'a t) (fn : string) (fv : 'a) : unit = Hashtbl.replace obj fn fv
let delete (obj : 'a t) (fn : string) : unit = Hashtbl.remove obj fn

let pp (pp_val : 'a pp_fmt) (fmt : Format.formatter) (obj : 'a t) : unit =
  let open Format in
  let pp_sep seq fmt () = pp_print_string fmt seq in
  let pp_seq seq pp fmt lst = pp_print_seq ~pp_sep:(pp_sep seq) pp fmt lst in
  let pp_fld fmt (fn, fv) = fprintf fmt "%S: %a" fn pp_val fv in
  fprintf fmt "{ %a }" (pp_seq ", " pp_fld) (Hashtbl.to_seq obj)

let str (pp_val : 'a pp_fmt) (obj : 'a t) : string =
  Format.asprintf "%a" (pp pp_val) obj

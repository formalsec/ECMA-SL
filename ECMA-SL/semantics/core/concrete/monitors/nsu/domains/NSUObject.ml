open EslCore

type 'sl t = (string, 'sl * 'sl) Hashtbl.t

let create () : 'sl t = Hashtbl.create !Base.default_hashtbl_sz

let get (obj : 'sl t) (fn : string) : ('sl * 'sl) option =
  Hashtbl.find_opt obj fn

let set (obj : 'sl t) (fn : string) (exist_lvl : 'sl) (val_lvl : 'sl) : unit =
  Hashtbl.replace obj fn (exist_lvl, val_lvl)

let delete (obj : 'sl t) (fn : string) : unit = Hashtbl.remove obj fn

let str (sl_printer : 'sl -> string) (obj : 'sl t) : string =
  let fld_str prop exists_lvl val_lvl =
    Printf.sprintf "%s: (%s, %s)" prop (sl_printer exists_lvl)
      (sl_printer val_lvl)
  in
  let obj_str_f prop (exists_lvl, val_lvl) acc =
    fld_str prop exists_lvl val_lvl :: acc
  in
  Hashtbl.fold obj_str_f obj [] |> String.concat ", "

let new_sec_prop (obj : 'sl t) (field : string) (exists_lvl : 'sl)
  (val_lvl : 'sl) : unit =
  Hashtbl.replace obj field (exists_lvl, val_lvl)

let upg_exists (obj : 'sl t) (field : string) (lvl : 'sl) : unit =
  match get obj field with
  | Some (_, val_lvl) -> set obj field lvl val_lvl
  | None -> raise (NSUException.Exists "Cannot find object field")

let upg_val (obj : 'sl t) (field : string) (lvl : 'sl) : unit =
  match get obj field with
  | Some (exists_lvl, _) -> set obj field exists_lvl lvl
  | None -> raise (NSUException.Exists "Cannot find object field")

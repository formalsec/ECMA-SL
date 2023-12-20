exception Exists of string

type 'sl t = (string, 'sl * 'sl) Hashtbl.t
(*              Exists_lvl   Val_lvl    *)

let create () : 'sl t = Hashtbl.create 511
let get (obj : 'sl t) (f : string) : ('sl * 'sl) option = Hashtbl.find_opt obj f

let set (obj : 'sl t) (f : string) (exist_lvl : 'sl) (value_lvl : 'sl) : unit =
  Hashtbl.replace obj f (exist_lvl, value_lvl)

let delete (obj : 'sl t) (f : string) : unit = Hashtbl.remove obj f

let str (str_sl : 'sl -> string) (obj : 'sl t) : string =
  Hashtbl.fold
    (fun prop (exists_lvl, val_lvl) acc ->
      Printf.sprintf "%s: (%s, %s), %s" prop (str_sl exists_lvl)
        (str_sl val_lvl) acc )
    obj ""

let new_sec_prop (obj : 'sl t) (field : string) (exists_lvl : 'sl)
  (val_lvl : 'sl) : unit =
  Hashtbl.replace obj field (exists_lvl, val_lvl)

let upg_exists (obj : 'sl t) (field : string) (lvl : 'sl) : unit =
  match get obj field with
  | Some (_, val_lvl) -> set obj field lvl val_lvl
  | None -> raise (Exists "UpgExists" (*TODO*))

let upg_val (obj : 'sl t) (field : string) (lvl : 'sl) : unit =
  match get obj field with
  | Some (exists_lvl, _) -> set obj field exists_lvl lvl
  | None -> raise (Exists "UpgVal" (*TODO*))

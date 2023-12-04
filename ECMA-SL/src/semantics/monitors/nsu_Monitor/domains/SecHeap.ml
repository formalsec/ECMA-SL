exception Exists of string

type 'sl t = (Loc.t, 'sl SecObject.t * 'sl * 'sl) Hashtbl.t
(*                 Object       Struct_lvl   Object_lvl           *)

let create () : 'sl t = Hashtbl.create 511

let insert (heap : 'sl t) (loc : Loc.t) (secobj : 'sl SecObject.t)
    (struct_lvl : 'sl) (obj_lvl : 'sl) : unit =
  if Hashtbl.mem heap loc then raise (Exists "location already exists")
  else Hashtbl.replace heap loc (secobj, struct_lvl, obj_lvl)

let create_object (heap : 'sl t) (loc : Loc.t) (lvl : 'sl) : unit =
  let new_obj = SecObject.create () in
  insert heap loc new_obj lvl lvl

let delete (heap : 'sl t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : 'sl t) (loc : Loc.t) (secobj : 'sl SecObject.t)
    (struct_lvl : 'sl) (object_lvl : 'sl) : unit =
  Hashtbl.replace heap loc (secobj, struct_lvl, object_lvl)

let get (heap : 'sl t) (loc : Loc.t) : 'sl SecObject.t * 'sl * 'sl =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | None -> raise (Exists "Entry not found")
  | Some (sec_obj, struct_lvl, object_lvl) -> (sec_obj, struct_lvl, object_lvl)

let get_struct_lvl (heap : 'sl t) (loc : Loc.t) : 'sl option =
  let res = Hashtbl.find_opt heap loc in
  match res with Some (_, struct_lvl, _) -> Some struct_lvl | None -> None

let get_object_lvl (heap : 'sl t) (loc : Loc.t) : 'sl option =
  let res = Hashtbl.find_opt heap loc in
  match res with Some (_, _, object_lvl) -> Some object_lvl | None -> None

let get_obj (heap : 'sl t) (loc : Loc.t) : 'sl SecObject.t option =
  let res = Hashtbl.find_opt heap loc in
  match res with None -> None | Some (sec_obj, _, _) -> Some sec_obj

let get_field (heap : 'sl t) (loc : Loc.t) (field : Field.t) :
    ('sl * 'sl) option =
  let obj = get_obj heap loc in
  match obj with Some obj -> SecObject.get obj field | None -> None

let delete_field (heap : 'sl t) (loc : Loc.t) (field : Field.t) : bool =
  let obj = get_obj heap loc in
  match obj with
  | Some obj ->
      SecObject.delete obj field;
      true
  | None -> false

let new_sec_prop (heap : 'sl t) (loc : Loc.t) (field : Field.t)
    (exists_lvl : 'sl) (val_lvl : 'sl) : bool =
  match get_obj heap loc with
  | Some obj ->
      SecObject.new_sec_prop obj field exists_lvl val_lvl;
      true
  | None -> false

let newSecObj (heap : 'sl t) (loc : Loc.t) (struct_lvl : 'sl) (obj_lvl : 'sl) :
    unit =
  let sec_obj = SecObject.create () in
  insert heap loc sec_obj struct_lvl obj_lvl

let upg_prop_exists_lvl (heap : 'sl t) (loc : Loc.t) (field : Field.t)
    (lvl : 'sl) : unit =
  let sec_obj, _, _ = get heap loc in
  SecObject.upg_exists sec_obj field lvl

let upg_prop_val_lvl (heap : 'sl t) (loc : Loc.t) (field : Field.t) (lvl : 'sl)
    : unit =
  let sec_obj, _, _ = get heap loc in
  SecObject.upg_val sec_obj field lvl

let upg_object_lvl (heap : 'sl t) (loc : Loc.t) (lvl : 'sl) : unit =
  let sec_obj, struct_lvl, _ = get heap loc in
  update heap loc sec_obj struct_lvl lvl

let upg_struct_lvl (heap : 'sl t) (loc : Loc.t) (lvl : 'sl) : unit =
  let sec_obj, _, object_lvl = get heap loc in
  update heap loc sec_obj lvl object_lvl

let str (str_sl : 'sl -> string) (heap : 'sl t) : string =
  Hashtbl.fold
    (fun loc (obj, struct_lvl, object_lvl) acc ->
      Printf.sprintf "%s|-> {%s}_{%s, %s}\n" loc (SecObject.str str_sl obj)
        (str_sl struct_lvl) (str_sl object_lvl)
      ^ acc)
    heap ""

exception Exists of string

type t = (Loc.t, (SecObject.t * SecLevel.t * SecLevel.t)) Hashtbl.t
(*                 Object       Struct_lvl   Object_lvl           *)

let create () : t = Hashtbl.create 511

let insert  (heap : t) (loc : Loc.t) (secobj : SecObject.t) (struct_lvl : SecLevel.t) (obj_lvl : SecLevel.t) : unit =
  if(Hashtbl.mem heap loc) then raise (Exists "location already exists") else
    Hashtbl.replace heap loc (secobj, struct_lvl,  obj_lvl)

let delete (heap : t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : t) (loc : Loc.t) (secobj : SecObject.t) (struct_lvl : SecLevel.t) (obj_lvl : SecLevel.t) : unit =
  Hashtbl.replace heap loc (secobj, struct_lvl, obj_lvl)

let get (heap : t) (loc : Loc.t) : (SecObject.t * SecLevel.t * SecLevel.t) =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | None -> raise (Exists "Entry not found")
  | Some (sec_obj, struct_lvl, obj_lvl) ->  (sec_obj, struct_lvl, obj_lvl)

let get_struct (heap : t) (loc : Loc.t) : SecLevel.t option =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | Some (_, lev_struct, _) -> Some lev_struct
  | None -> None

let get_val (heap : t) (loc : Loc.t) : SecLevel.t option =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | Some (_, _, lev_val) -> Some lev_val
  | None -> None

let get_obj (heap : t) (loc : Loc.t) : SecObject.t option =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | None -> None
  | Some (sec_obj, _, _) -> Some sec_obj

let get_field (heap : t) (loc : Loc.t) (field : Field.t) : (SecLevel.t * SecLevel.t) option =
  let obj = get_obj heap loc in
  match obj with
  | Some obj -> Some (SecObject.get obj field)
  | None -> None

let delete_field (heap : t) (loc : Loc.t) (field : Field.t) : bool =
  let obj = get_obj heap loc in
  match obj with
  | Some obj -> SecObject.delete obj field; true
  | None -> false

let new_sec_prop (heap : t) (loc : Loc.t) (field : Field.t) (exists_lvl : SecLevel.t) (val_lvl : SecLevel.t) : bool =
  match get_obj heap loc with
  | Some obj -> SecObject.new_sec_prop obj field exists_lvl val_lvl; true
  | None -> false


let (*Verificar onde é chamado*) newSecObj (heap : t) (loc : Loc.t) (struct_lvl : SecLevel.t) (obj_lvl : SecLevel.t)  : unit =
  let sec_obj = SecObject.create () in
  insert heap loc sec_obj struct_lvl obj_lvl

let upg_prop_exists (heap : t) (loc : Loc.t) (field : Field.t ) (lvl : SecLevel.t) : unit =
  let (sec_obj, struct_lvl, obj_lvl) = get heap loc in
  SecObject.upg_exists sec_obj field lvl

let upg_prop_val (heap : t) (loc : Loc.t) (field : Field.t) (lvl : SecLevel.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  SecObject.upg_val sec_obj field lvl

let upg_struct_val (heap : t) (loc : Loc.t) (lvl : SecLevel.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  update heap loc sec_obj lvl  exist_lvl

let upg_struct_exists (heap : t) (loc :Loc.t) (lvl : SecLevel.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  update heap loc sec_obj strut_lvl lvl

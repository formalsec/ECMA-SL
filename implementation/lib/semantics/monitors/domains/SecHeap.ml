exception Exists of string

type t = (Loc.t, (SecObject.t * SecLevel.t * SecLevel.t)) Hashtbl.t

let create () : t = Hashtbl.create 511

let insert  (heap : t) (loc : Loc.t) (secobj : SecObject.t) (obj_lvl : SecLevel.t) (exist_lvl : SecLevel.t) : unit =
  if(Hashtbl.mem heap loc) then raise (Exists "location already exists") else
    Hashtbl.add heap loc (secobj,obj_lvl,exist_lvl)

let delete (heap : t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : t) (loc : Loc.t) (secobj : SecObject.t) (obj_lvl : SecLevel.t) (exist_lvl : SecLevel.t) : unit =
  Hashtbl.replace heap loc (secobj,obj_lvl,exist_lvl)

let get (heap : t) (loc : Loc.t) : (SecObject.t * SecLevel.t * SecLevel.t) =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | None -> raise (Exists "Entry not found")
  | Some (sec_obj,obj_lvl,struct_lvl) ->  (sec_obj,obj_lvl,struct_lvl)

let get_struct (heap : t) (loc : Loc.t) : SecLevel.t option =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | Some (_, lev_struct, _) -> Some lev_struct
  | None -> None

let get_obj (heap : t) (loc : Loc.t) : SecObject.t option =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | None -> None
  | Some (sec_obj,obj_lvl,struct_lvl) -> Some sec_obj

let get_field (heap : t) (loc : Loc.t) (field : Field.t) : (SecLevel.t * SecLevel.t) option =
  (*Falta fazer isto*)
  let obj = get_obj heap loc in
  match obj with
  | Some obj -> Some (SecObject.get obj field)
  | None -> None

let delete_field (heap : t) (loc : Loc.t) (field : Field.t) : bool =
  let obj = get_obj heap loc in
  match obj with
  | Some obj -> SecObject.delete obj field; true
  | None -> false


let newSecObj (heap : t) (loc : Loc.t) (obj_lvl : SecLevel.t) (struct_lvl : SecLevel.t) : unit =
  let sec_obj = SecObject.create () in
  insert heap loc sec_obj obj_lvl struct_lvl

let upg_prop_exists (heap : t) (loc : Loc.t) (prop : Field.t ) (lvl : SecLevel.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  let (prop_val_lvl,prop_exist_lvl) = SecObject.get sec_obj  prop in
  SecObject.set sec_obj prop prop_val_lvl lvl

let upg_prop_val (heap : t) (loc : Loc.t) (prop : Field.t) (lvl : SecLevel.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  let (prop_val_lvl,prop_exist_lvl) = SecObject.get sec_obj prop in
  SecObject.set sec_obj prop lvl prop_exist_lvl

let upg_struct_val (heap : t) (loc : Loc.t) (lvl : SecLevel.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  update heap loc sec_obj lvl  exist_lvl

let upg_struct_exists (heap : t) (loc :Loc.t) (lvl : SecLevel.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  update heap loc sec_obj strut_lvl lvl

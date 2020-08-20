exception Exists of string

type t = (Loc.t, (SecObject.t * Level.t * Level.t)) Hashtbl.t

let create () : t = Hashtbl.create 511

let insert  (heap : t) (loc : Loc.t) (secobj : SecObject.t) (obj_lvl : Level.t) (exist_lvl : Level.t) : unit =
  if(Hashtbl.mem heap loc) then raise (Exists "location already exists") else
    Hashtbl.add heap loc (secobj,obj_lvl,exist_lvl);
  loc

let remove (heap : t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : t) (loc : Loc.t) (secobj : SecObject.t) (obj_lvl : Level.t) (exist_lvl : Level.t) : unit =
  Hashtbl.replace heap loc (secobj,obj_lvl,exist_lvl)

let get (heap : t) (loc : Loc.t) : SecObject.t option =
  Hashtbl.find_opt heap loc

let newSecObj (heap: t) (loc:Loc.t) (obj_lvl:Level.t) (struct_lvl:Level.t) : unit =
  let sec_obj = SecObject.create () in
  insert heap loc sec_obj obj_lvl struct_lvl

let upg_prop_exist (heap : t) (loc : Loc.t) (prop : Field.t ) (lvl : Level.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  let (prop_val_lvl,prop_exist_lvl) = SecObject.get sec_obj in
  SecObject.set sec_obj (prop_val_lvl, lvl)

let upg_prop_val (heap : t) (loc : Loc.t) (prop: Field.t) (lvl : Level.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  let (prop_val_lvl,prop_exist_lvl) = SecObject.get sec_obj in
  SecObject.set sec_obj (lvl, prop_exist_lvl)

let upg_struct_val_lvl (heap : t) (loc : Loc.t) (lvl : Level.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  update heap loc (sec_obj,lvl, exist_lvl)

let upg_struct_exists_lvl (heap : t) (lvl : Level.t) : unit =
  let (sec_obj,strut_lvl,exist_lvl) = get heap loc in
  update heap loc (sec_obj,strut_lvl, lvl)

exception Exists of string

type t = (Loc.t, (SecObject.t * SecLevel.t * SecLevel.t)) Hashtbl.t

let create () : t = Hashtbl.create 511

let insert  (heap : t) (loc : Loc.t) (secobj : SecObject.t) (obj_lvl : SecLevel.t) (exist_lvl : SecLevel.t) : unit =
  if(Hashtbl.mem heap loc) then raise (Exists "location already exists") else
    Hashtbl.add heap loc (secobj,obj_lvl,exist_lvl)

let remove (heap : t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : t) (loc : Loc.t) (secobj : SecObject.t) (obj_lvl : SecLevel.t) (exist_lvl : SecLevel.t) : unit =
  Hashtbl.replace heap loc (secobj,obj_lvl,exist_lvl)

let get (heap : t) (loc : Loc.t) : (SecObject.t * SecLevel.t * SecLevel.t) =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | None -> raise (Exists "Entry not found")
  | Some (sec_obj,obj_lvl,struct_lvl) ->  (sec_obj,obj_lvl,struct_lvl)

let get_obj (heap : t) (loc : Loc.t) : SecObject.t option =
  let res = Hashtbl.find_opt heap loc in
  match res with
  | None -> None
  | Some (sec_obj,obj_lvl,struct_lvl) -> Some sec_obj

let get_prop (heap : t) (loc : Loc.t) (f : Field.t) : SecLevel.t =
  (*Falta fazer isto*)
  SecLevel.Low

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

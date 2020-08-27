exception Exists of string

type t = (Field.t, (SecLevel.t * SecLevel.t)) Hashtbl.t
(*              Exists_lvl   Val_lvl    *)

let create () : t = Hashtbl.create 511

let get (obj : t) (f : Field.t) : (SecLevel.t * SecLevel.t) =
  let res = Hashtbl.find_opt obj f in
  match res with
  | None -> raise (Exists "Entry not found")
  | Some (exist_lvl, value_lvl) ->  (exist_lvl, value_lvl)

let set (obj : t) (f : Field.t) (exist_lvl : SecLevel.t) (value_lvl : SecLevel.t) : unit =
  Hashtbl.replace obj f (exist_lvl,value_lvl)

let delete (obj : t) (f : Field.t) : unit =
  Hashtbl.remove obj f

let str (obj : t) : string =
  (Hashtbl.fold (fun k v ac ->
       match v with
       | (exist_lvl, value_lvl) ->
         (if ac <> "{ " then ac ^ ", " else ac) ^ (Printf.sprintf "%s: %s,%s" (Field.str k) (SecLevel.str exist_lvl)(SecLevel.str value_lvl))) obj "{ ") ^ " }"

let new_sec_prop (obj : t) (field : Field.t) (exists_lvl : SecLevel.t) (val_lvl : SecLevel.t) : unit =
  Hashtbl.replace obj field (exists_lvl, val_lvl)

let upg_exists (obj : t) (field : Field.t) (lvl : SecLevel.t) : unit =
  let (exist_lvl',val_lvl') = get obj field in
  set obj field lvl val_lvl'

let upg_val (obj : t) (field : Field.t) (lvl : SecLevel.t) : unit =
  let (exist_lvl',val_lvl') = get obj field in
  set obj field exist_lvl' lvl

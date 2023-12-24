type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : (Loc.t, 'a obj) Hashtbl.t
  }

let create () : 'a t =
  { parent = None; map = Hashtbl.create !Config.default_hashtbl_sz }

let clone (heap : 'a t) : 'a t =
  { parent = Some heap; map = Hashtbl.create !Config.default_hashtbl_sz }

let insert (heap : 'a t) (obj : 'a obj) : Loc.t =
  let loc = Loc.newloc () in
  Hashtbl.add heap.map loc obj;
  loc

let remove (heap : 'a t) (loc : Loc.t) : unit = Hashtbl.remove heap.map loc

let set (heap : 'a t) (loc : Loc.t) (obj : 'a obj) : unit =
  Hashtbl.replace heap.map loc obj

let rec get (heap : 'a t) (loc : Loc.t) : 'a obj option =
  match Hashtbl.find_opt heap.map loc with
  | Some _ as obj -> obj
  | None ->
    let obj = Option.map_default (fun h -> get h loc) None heap.parent in
    Option.may (fun o -> set heap loc (Object.clone o)) obj;
    obj

let get_field (heap : 'a t) (loc : Loc.t) (fn : string) : 'a option =
  get heap loc |> Option.map_default (fun o -> Object.get o fn) None

let set_field (heap : 'a t) (loc : Loc.t) (fn : string) (v : 'a) : unit =
  get heap loc |> Option.may (fun o -> Object.set o fn v)

let delete_field (heap : 'a t) (loc : Loc.t) (fn : string) : unit =
  get heap loc |> Option.may (fun o -> Object.delete o fn)

let str (val_printer : Val.t -> string) (heap : 'a t) : string =
  let _loc_str l = Loc.str l in
  let _obj_str o = Object.str val_printer o in
  let _binding_str l o = Printf.sprintf "%s: %s" (_loc_str l) (_obj_str o) in
  let _heap_str_f l o acc = _binding_str l o :: acc in
  let heap_str = Hashtbl.fold _heap_str_f heap.map [] |> String.concat ", " in
  "{ " ^ heap_str ^ " }"

let str_with_glob (val_printer : Val.t -> string) (heap : 'a t) : string =
  (* TODO: Return the heap with the __$global object (i.e., special object that \
     contains the field __$global) *)
  str val_printer heap

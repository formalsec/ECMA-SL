open Syntax.Option

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
  let l = Loc.create () in
  Hashtbl.add heap.map l obj;
  l

let set (heap : 'a t) (l : Loc.t) (obj : 'a obj) : unit =
  Hashtbl.replace heap.map l obj

let rec get_opt (heap : 'a t) (l : Loc.t) : 'a obj option =
  match Hashtbl.find_opt heap.map l with
  | Some _ as obj -> obj
  | None ->
    let* parent = heap.parent in
    let+ obj = get_opt parent l in
    let obj' = Object.clone obj in
    set heap l obj';
    obj'

let get (heap : 'a t) (l : Loc.t) : ('a obj, string) Result.t =
  match get_opt heap l with
  | Some obj -> Ok obj
  | None -> Error (Format.sprintf "Cannot find lation '%s'." l)

let get_field_opt (heap : 'a t) (l : Loc.t) (fn : string) : 'a option =
  let* obj = get_opt heap l in
  Object.get obj fn

let str (val_printer : Val.t -> string) (heap : 'a t) : string =
  let _str_loc l = Loc.str l in
  let _str_obj o = Object.str val_printer o in
  let _str_binding l o = Printf.sprintf "%s: %s" (_str_loc l) (_str_obj o) in
  let _str_heap_f l o acc = _str_binding l o :: acc in
  let heap_str = Hashtbl.fold _str_heap_f heap.map [] |> String.concat ", " in
  "{ " ^ heap_str ^ " }"

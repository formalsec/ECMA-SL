open Core

type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : (Loc.t, 'a obj) Hashtbl.t
  }

let create () : 'a t = { parent = None; map = Hashtbl.create (module String) }

let clone (h : 'a t) : 'a t =
  { parent = Some h; map = Hashtbl.create (module String) }

let insert (h : 'a t) (obj : 'a obj) : Loc.t =
  let loc = Loc.newloc () in
  Hashtbl.set h.map ~key:loc ~data:obj;
  loc

let remove (h : 'a t) (l : Loc.t) : unit = Hashtbl.remove h.map l

let set (h : 'a t) (key : Loc.t) (data : 'a obj) : unit =
  Hashtbl.set h.map ~key ~data

let rec get (h : 'a t) (l : Loc.t) : 'a obj option =
  match Hashtbl.find h.map l with
  | Some _ as v -> v
  | None ->
    let obj = Option.bind h.parent ~f:(fun h -> get h l) in
    Option.iter obj ~f:(fun o -> set h l (Object.clone o));
    obj

let get_field (heap : 'a t) (loc : Loc.t) (field : String.t) : 'a option =
  let obj = get heap loc in
  Option.bind obj ~f:(fun o -> Object.get o field)

let set_field (heap : 'a t) (loc : Loc.t) (field : String.t) (v : 'a) : unit =
  let obj = get heap loc in
  Option.iter obj ~f:(fun o -> Object.set o field v)

let delete_field (heap : 'a t) (loc : Loc.t) (field : String.t) : unit =
  let obj = get heap loc in
  Option.iter obj ~f:(fun o -> Object.delete o field)

let to_string (h : 'a t) (pp : 'a -> string) : string =
  "{ "
  ^ String.concat ~sep:", "
      (Hashtbl.fold h.map ~init:[] ~f:(fun ~key:n ~data:v acc ->
           Printf.sprintf "%s: %s" (Loc.str n) (Object.str v pp) :: acc )
      )
  ^ " }"

let to_string_with_glob (h : 'a t) (pp : 'a -> string) : string =
  let glob =
    Hashtbl.fold h.map ~init:None ~f:(fun ~key:_ ~data:obj acc ->
        match acc with
        | Some _ -> acc
        (* Keep this in sync with Compiler.ml function *)
        (* "compile_gvar" and "compile_glob_assign" *)
        | None -> Object.get obj Common.global_var_compiled )
  in
  match glob with
  | Some l ->
    Printf.sprintf "{ \"heap\": %s, \"global\": %s }" (to_string h pp)
      (Val.str l)
  | None ->
    raise
      (Failure
         "Couldn't find the Object that contains only one property, named \
          \"global\"." )

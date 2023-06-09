open Core

type encoded_pct = Encoding.Expression.t
type 'a obj = 'a S_object.t
type 'a t = { parent : 'a t option; map : (Loc.t, 'a obj) Hashtbl.t }

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
      Option.iter obj ~f:(fun o -> set h l (S_object.clone o));
      obj

let get_field (heap : 'a t) (loc : Loc.t) (field : Expr.t)
    (solver : Encoding.Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    ('a t * 'a obj * encoded_pct option * 'a option) list =
  let obj = get heap loc in
  let res =
    Option.bind obj ~f:(fun o -> Some (S_object.get o field solver pc store))
  in
  match res with
  | None -> failwith ("get Return is never none. loc: " ^ loc ^ Expr.str field)
  | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc, v) ] ->
          if Option.is_some pc then set heap loc obj;
          [ (heap, obj, pc, v) ]
      | _ ->
          List.map objs ~f:(fun (obj, pc, v) ->
              let heap' = clone heap in
              set heap' loc obj;
              (heap', obj, pc, v)))

let set_field (heap : 'a t) (loc : Loc.t) (field : Expr.t) (v : 'a)
    (solver : Encoding.Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    ('a t * 'a obj * encoded_pct option) list =
  let obj = get heap loc in
  let res =
    Option.bind obj ~f:(fun o -> Some (S_object.set o field v solver pc store))
  in
  match res with
  | None -> failwith ("set Return is never none. loc: " ^ loc)
  | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc) ] ->
          if Option.is_some pc then set heap loc obj;
          [ (heap, obj, pc) ]
      | _ ->
          List.map objs ~f:(fun (obj, pc) ->
              let heap' = clone heap in
              set heap' loc obj;
              (heap', obj, pc)))

let delete_field (heap : 'a t) (loc : Loc.t) (field : Expr.t)
    (solver : Encoding.Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    ('a t * 'a obj * encoded_pct option) list =
  let obj = get heap loc in
  let res =
    Option.bind obj ~f:(fun o -> Some (S_object.delete o field solver pc store))
  in

  match res with
  | None -> failwith ("delete Return is never none. loc: " ^ loc)
  | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc') ] ->
          if Option.is_some pc' then set heap loc obj;
          [ (heap, obj, pc') ]
      | _ ->
          List.map objs ~f:(fun (obj, pc) ->
              let heap' = clone heap in
              set heap' loc obj;
              (heap', obj, pc)))

let to_string (h : 'a t) (pp : 'a -> string) : string =
  "{ "
  ^ String.concat ~sep:", "
      (Hashtbl.fold h.map ~init:[] ~f:(fun ~key:n ~data:v acc ->
           Printf.sprintf "%s: %s" (Loc.str n) (S_object.to_string v pp) :: acc))
  ^ " }"

let to_string_with_glob (h : 'a t) (pp : 'a -> string) : string =
  let glob =
    Hashtbl.fold h.map ~init:None ~f:(fun ~key:_ ~data:obj acc ->
        match acc with
        | Some _ -> acc
        (* Keep this in sync with Compiler.ml function *)
        (* "compile_gvar" and "compile_glob_assign" *)
        | None -> S_object.get_concrete_field obj Common.global_var_compiled)
  in
  match glob with
  | Some l ->
      Printf.sprintf "{ \"heap\": %s, \"global\": %s }" (to_string h pp)
        (Val.str l)
  | None ->
      raise
        (Failure
           "Couldn't find the Object that contains only one property, named \
            \"global\".")

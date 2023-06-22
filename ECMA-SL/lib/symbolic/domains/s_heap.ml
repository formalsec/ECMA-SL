open Core

module MakeHeap(Object : S_obj.SymbolicObject) = struct
  type encoded_pct = Encoding.Expression.t
  type obj = Object.t
  type t = { parent : t option; map : (Loc.t, obj) Hashtbl.t }

  let create () : t = { parent = None; map = Hashtbl.create (module String) }

  let clone (h : t) : t =
    { parent = Some h; map = Hashtbl.create (module String) }

  let insert (h : t) (obj : obj) : Loc.t =
    let loc = Loc.newloc () in
    Hashtbl.set h.map ~key:loc ~data:obj;
    loc

  let remove (h : t) (l : Loc.t) : unit = Hashtbl.remove h.map l

  let set (h : t) (key : Loc.t) (data : obj) : unit =
    Hashtbl.set h.map ~key ~data

  let rec get (h : t) (l : Loc.t) : obj option =
    let result = Hashtbl.find h.map l in
    match result with
    | Some o -> result
    | None -> (
        let obj = Option.bind h.parent ~f:(fun h -> get h l) in
        match obj with
        | Some o ->
            let o' = Object.clone o in
            set h l o';
            Some o'
        | None -> None)

let has_field (h : t) (l : Loc.t) (f : Expr.t) : Expr.t =
  Option.value_map (get h l) ~default:(Expr.Val (Val.Bool false)) ~f:(fun o ->
      Object.has_field o f)

  let get_field (heap : t) (loc : Loc.t) (field : Expr.t)
      (solver : Encoding.Batch.t) (pc : encoded_pct list) (store : Sstore.t) :
      (t * encoded_pct list * 'a option) list =
    let obj = get heap loc in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.get o field solver pc store))
    in
    match res with
    | None -> failwith ("get Return is never none. loc: " ^ loc ^ Expr.str field)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc, v) ] ->
            set heap loc obj;
            [ (heap, pc, v) ]
        | _ ->
            List.map objs ~f:(fun (obj, pc, v) ->
                let heap' = clone heap in
                set heap' loc obj;
                (heap', pc, v)))

  let set_field (heap : t) (loc : Loc.t) (field : Expr.t) (v : 'a)
      (solver : Encoding.Batch.t) (pc : encoded_pct list) (store : Sstore.t) :
      (t * encoded_pct list) list =
    let obj = get heap loc in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.set o field v solver pc store))
    in
    match res with
    | None -> failwith ("set Return is never none. loc: " ^ loc)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc) ] ->
            set heap loc obj;
            [ (heap, pc) ]
        | _ ->
            List.map objs ~f:(fun (obj, pc) ->
                let heap' = clone heap in
                set heap' loc obj;
                (heap', pc)))

  let delete_field (heap : t) (loc : Loc.t) (field : Expr.t)
      (solver : Encoding.Batch.t) (pc : encoded_pct list) (store : Sstore.t) :
      (t * encoded_pct list) list =
    let obj = get heap loc in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.delete o field solver pc store))
    in

    match res with
    | None -> failwith ("delete Return is never none. loc: " ^ loc)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc') ] ->
            set heap loc obj;
            [ (heap, pc') ]
        | _ ->
            List.map objs ~f:(fun (obj, pc) ->
                let heap' = clone heap in
                set heap' loc obj;
                (heap', pc)))

  (* let to_string (h : 'a t) (pp : 'a -> string) : string =
    "{ "
    ^ String.concat ~sep:", "
        (Hashtbl.fold h.map ~init:[] ~f:(fun ~key:n ~data:v acc ->
            Printf.sprintf "%s: %s" (Loc.str n) (S_object.to_string v pp) :: acc))
    ^ " }" *)

  (* let to_string_with_glob (h : 'a t) (pp : 'a -> string) : string =
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
              \"global\".") *)
end
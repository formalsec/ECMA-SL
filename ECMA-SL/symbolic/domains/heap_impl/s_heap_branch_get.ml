open Core

module Object = S_object_branch_get
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


let get_field (heap : t) (loc : Expr.t) (field : Expr.t)
    (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    (t * encoded_pct list * 'a option) list =
  match loc with
  | Expr.Val (Val.Loc l) ->(
    let obj = get heap l in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.get o field solver pc store))
    in
    match res with
    | None -> failwith ("get Return is never none. loc: " ^ l ^ Expr.str field)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc, v) ] ->
            set heap l obj;
            [ (heap, pc, v) ]
        | _ ->
            List.map objs ~f:(fun (obj, pc, v) ->
                let heap' = clone heap in
                set heap' l obj;
                (heap', pc, v)))
  )
  | _ -> failwith "invalid loc"

let has_field (heap : t) (loc : Expr.t) (field : Expr.t)
  (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
  (t * encoded_pct list * 'a) list =
  match loc with
  | Expr.Val (Val.Loc l) ->
    let res = get_field heap loc field solver pc store in
    List.map res ~f:(fun (new_heap, new_pc, v) ->
      let v' = Expr.Val (Val.Bool (Option.is_some v)) in
      new_heap, new_pc, v'
    )
  | _ -> failwith "invalid loc expr."

let assign_obj_to_list (heap : t) (loc : Expr.t)
  (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
  Expr.t =
  match loc with
  | Expr.Val (Val.Loc l) -> (
    let obj = get heap l in
    match obj with
    | None -> failwith "Object not found."
    | Some o ->
        let ret =
          Expr.NOpt
            ( Operators.ListExpr,
              List.map (Object.to_list o) ~f:(fun (f, v) ->
                  Expr.NOpt (Operators.TupleExpr, [ f; v ])) )
        in
        ret
  )
  | _ -> let msg = Printf.sprintf "Invalid loc expr %s" (Expr.str loc) in failwith msg 

let assign_obj_fields (heap : t) (loc : Expr.t)
  (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
  Expr.t =
  match loc with
  | Expr.Val (Val.Loc l) -> (
    let obj = get heap l in
    match obj with
    | None -> failwith "Object not found."
    | Some o -> 
      Expr.NOpt (Operators.ListExpr, Object.get_fields o)
  )
  | _ -> let msg = Printf.sprintf "Invalid loc expr %s" (Expr.str loc) in failwith msg 

let set_field (heap : t) (loc : Expr.t) (field : Expr.t) (v : 'a)
    (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    (t * encoded_pct list) list =
  match loc with
  | Expr.Val (Val.Loc l) -> (
    let obj = get heap l in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.set o field v solver pc store))
    in
    match res with
    | None -> failwith ("set Return is never none. loc: " ^ l)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc) ] ->
            set heap l obj;
            [ (heap, pc) ]
        | _ ->
            List.map objs ~f:(fun (obj, pc) ->
                let heap' = clone heap in
                set heap' l obj;
                (heap', pc)))
  )
  | _ -> let msg = Printf.sprintf "invalid loc expr %s" (Expr.str loc) in failwith msg 

let delete_field (heap : t) (loc : Expr.t) (field : Expr.t)
    (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    (t * encoded_pct list) list =
    match loc with
  | Expr.Val (Val.Loc l) -> (
    let obj = get heap l in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.delete o field solver pc store))
    in
    match res with
    | None -> failwith ("delete Return is never none. loc: " ^ l)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc') ] ->
            set heap l obj;
            [ (heap, pc') ]
        | _ ->
            List.map objs ~f:(fun (obj, pc) ->
                let heap' = clone heap in
                set heap' l obj;
                (heap', pc)))
    )
  | _ -> let msg = Printf.sprintf "invalid loc expr %s" (Expr.str loc) in failwith msg 


(* let to_string (h : t) (pp : 'a -> string) : string =
  "{ "
  ^ String.concat ~sep:", "
      (Hashtbl.fold h.map ~init:[] ~f:(fun ~key:n ~data:v acc ->
          Printf.sprintf "%s: %s" (Loc.str n) (S_object.to_string v pp) :: acc))
  ^ " }" *)

(* let to_string_with_glob (h : t) (pp : 'a -> string) : string =
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

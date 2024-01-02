open Core
module Value = Sym_value.M
module Object = Sym_object_list.M
module Reducer = Value_reducer
module Translator = Value_translator

module M : sig
  type value = Value.value
  type object_ = Object.t
  type encoded_pct = Encoding.Expr.t
  type t

  val create : unit -> t
  val clone : t -> t
  val insert : t -> object_ -> Loc.t
  val remove : t -> Loc.t -> unit
  val set : t -> Loc.t -> object_ -> unit
  val get : t -> Loc.t -> object_ option
  val assign_obj_fields : t -> value -> Batch.t -> encoded_pct list -> value
  val assign_obj_to_list : t -> value -> Batch.t -> encoded_pct list -> value

  val has_field :
       t
    -> loc:value
    -> field:value
    -> Batch.t
    -> encoded_pct list
    -> (t * encoded_pct list * value) list

  val get_field :
       t
    -> loc:value
    -> field:value
    -> Batch.t
    -> encoded_pct list
    -> (t * encoded_pct list * value option) list

  val set_field :
       t
    -> loc:value
    -> field:value
    -> data:value
    -> Batch.t
    -> encoded_pct list
    -> (t * encoded_pct list) list

  val delete_field :
       t
    -> loc:value
    -> field:value
    -> Batch.t
    -> encoded_pct list
    -> (t * encoded_pct list) list

  val to_string : t -> string
end = struct
  type value = Value.value
  type object_ = Object.t
  type encoded_pct = Encoding.Expr.t

  type t =
    { parent : t option
    ; map : (Loc.t, object_) Hashtbl.t
    }

  let create () : t = { parent = None; map = Hashtbl.create (module String) }

  let clone (h : t) : t =
    { parent = Some h; map = Hashtbl.create (module String) }

  let insert (h : t) (obj : object_) : Loc.t =
    let loc = Loc.create () in
    Hashtbl.set h.map ~key:loc ~data:obj;
    loc

  let remove (h : t) (l : Loc.t) : unit = Hashtbl.remove h.map l

  let set (h : t) (key : Loc.t) (data : object_) : unit =
    Hashtbl.set h.map ~key ~data

  let rec get (h : t) (l : Loc.t) : object_ option =
    let result = Hashtbl.find h.map l in
    match result with
    | Some _o -> result
    | None -> (
      let obj = Option.bind h.parent ~f:(fun h -> get h l) in
      match obj with
      | Some o ->
        let o' = Object.clone o in
        set h l o';
        Some o'
      | None -> None )

  let mk_ite (e1 : value) (e2 : value) (e3 : value) : value =
    Value.TriOpt (Operator.ITE, e1, e2, e3)

  let mk_not (e : value) : value = Value.UnOpt (Operator.LogicalNot, e)
  let mk_bool (b : bool) : value = Value.Val (Val.Bool b)

  let apply_op_get ~(cond : value) ~(left : value) ~(right : value)
    (solver : Batch.t) (op : value -> encoded_pct list -> value)
    (pc : encoded_pct list) : 'a =
    let encoded_guard = Translator.translate cond in
    let pc_left = encoded_guard :: pc in
    let encoded_guard = Translator.translate (mk_not cond) in
    let pc_right = encoded_guard :: pc in
    let cs = Batch.check solver in

    match (cs pc_left, cs pc_right) with
    | (true, true) ->
      let vl = op left pc_left in
      let vr = op right pc_right in
      mk_ite cond vl vr
    | (true, false) -> op left pc_left
    | (false, true) -> op right pc_right
    | _ -> failwith "Apply op error."

  let apply_op_set (h : t) ~(cond : value) ~(left : value) ~(right : value)
    (solver : Batch.t)
    (op :
         value
      -> encoded_pct list
      -> encoded_pct option
      -> t
      -> (t * encoded_pct list) list ) (pc : encoded_pct list) :
    (t * encoded_pct list) list =
    let encoded_guard_l = Translator.translate cond in
    let pc_l = encoded_guard_l :: pc in
    let encoded_guard_r = Translator.translate (mk_not cond) in
    let pc_r = encoded_guard_r :: pc in
    let cs = Batch.check solver in

    match (cs pc_l, cs pc_r) with
    | (true, true) ->
      op left pc_l (Some encoded_guard_l) (clone h)
      @ op right pc_r (Some encoded_guard_r) (clone h)
    | (true, false) -> op left pc_l (Some encoded_guard_l) (clone h)
    | (false, true) -> op right pc_r (Some encoded_guard_r) (clone h)
    | _ -> failwith "No path is valid in Set."

  let rec assign_obj_fields (h : t) (loc : value) (solver : Batch.t)
    (pc : encoded_pct list) : value =
    match loc with
    | Value.Val (Val.Loc l) -> (
      let obj = get h l in
      match obj with
      | None -> failwith "Object not found."
      | Some o -> Value.NOpt (Operator.ListExpr, Object.get_fields o) )
    | Value.TriOpt (Operator.ITE, cond, left, right) ->
      let op l pc = assign_obj_fields h l solver pc in
      apply_op_get ~cond ~left ~right solver op pc
    | Value.Val (Val.Symbol "undefined") -> assert false
    | _ -> assert false

  let rec assign_obj_to_list (h : t) (loc : value) (solver : Batch.t)
    (pc : encoded_pct list) : value =
    match loc with
    | Value.Val (Val.Loc l) -> (
      let obj = get h l in
      match obj with
      | None -> failwith "Object not found."
      | Some o -> Value.mk_list @@ List.map (Object.to_list o) ~f:Value.mk_tuple
      )
    | Value.TriOpt (Operator.ITE, cond, left, right) ->
      let op l pc = assign_obj_to_list h l solver pc in
      apply_op_get ~cond ~left ~right solver op pc
    | Value.Val (Val.Symbol "undefined") -> failwith "impossible"
    | _ -> assert false

  let rec has_field_aux (h : t) (loc : value) (field : value) (solver : Batch.t)
    (pc : encoded_pct list) : value =
    match loc with
    | Value.Val (Val.Loc l) ->
      Option.value_map (get h l) ~default:(mk_bool false) ~f:(fun o ->
          Object.has_field o field solver pc )
    | Value.TriOpt (Operator.ITE, cond, left, right) ->
      let op l pc = has_field_aux h l field solver pc in
      apply_op_get ~cond ~left ~right solver op pc
    | Value.Val (Val.Symbol "undefined") -> mk_bool false
    | _ -> assert false

  let has_field (h : t) ~(loc : value) ~(field : value) (solver : Batch.t)
    (pc : encoded_pct list) : (t * encoded_pct list * value) list =
    [ (h, [], has_field_aux h loc field solver pc) ]

  let rec get_field_aux (heap : t) (loc : value) (field : value)
    (solver : Batch.t) (pc : encoded_pct list) : 'a =
    match loc with
    | Value.Val (Val.Loc l) -> (
      let obj = get heap l in
      match obj with
      | None -> failwith "Object not found."
      | Some o -> Object.get o field solver pc )
    | Value.TriOpt (Operator.ITE, cond, left, right) ->
      let op l pc = get_field_aux heap l field solver pc in
      apply_op_get ~cond ~left ~right solver op pc
    | Value.Val (Val.Symbol "undefined") ->
      Format.kasprintf failwith "Get failed: |field:%a| \"undefined\""
        Value.Pp.pp field
    | _ -> assert false

  let get_field (heap : t) ~(loc : value) ~(field : value) (solver : Batch.t)
    (pc : encoded_pct list) : (t * encoded_pct list * value option) list =
    [ (heap, [], Some (get_field_aux heap loc field solver pc)) ]

  let set_field_exec (heap : t) (loc : Loc.t) (field : value) (v : 'a) : t =
    let obj = get heap loc in
    match obj with
    | None -> failwith ("set Return is never none. loc: " ^ loc)
    | Some o ->
      let o = Object.set o ~key:field ~data:v in
      set heap loc o;
      heap

  (* FIXME: encoded_guard is useless? *)
  let rec set_field_aux (* ?(encoded_guard = None) *) (heap : t) (loc : value)
    (field : value) (v : 'a) (solver : Batch.t) (pc : encoded_pct list) :
    (t * encoded_pct list) list =
    match loc with
    | Value.Val (Val.Loc l) -> [ (set_field_exec heap l field v, []) ]
    | Value.TriOpt (Operator.ITE, cond, left, right) ->
      let op l pc _guard h =
        set_field_aux (* ~encoded_guard:guard *) h l field v solver pc
      in
      apply_op_set heap ~cond ~left ~right solver op pc
    | Value.Val (Val.Symbol "undefined") ->
      failwith "Attempting to set undefined."
    | _ -> assert false

  let set_field (heap : t) ~(loc : value) ~(field : value) ~data:(v : value)
    (solver : Batch.t) (pc : encoded_pct list) : (t * encoded_pct list) list =
    set_field_aux heap loc field v solver pc

  let delete_field_exec (heap : t) (loc : Loc.t) (field : value) : t =
    let obj = get heap loc in
    match obj with
    | None -> failwith ("Delete obj is never none. loc: " ^ loc)
    | Some o ->
      let o' = Object.delete o field in
      set heap loc o';
      heap

  (* FIXME: useless encoded_guard? *)
  let rec delete_field_aux (* ?(encoded_guard = None) *) (heap : t)
    (loc : value) (field : value) (solver : Batch.t) (pc : encoded_pct list) :
    (t * encoded_pct list) list =
    match loc with
    | Value.Val (Val.Loc l) -> [ (delete_field_exec heap l field, []) ]
    | Value.TriOpt (Operator.ITE, cond, left, right) ->
      let op l pc _guard h =
        delete_field_aux (* ~encoded_guard:guard *) h l field solver pc
      in
      apply_op_set heap ~cond ~left ~right solver op pc
    | Value.Val (Val.Symbol "undefined") -> failwith "del woops"
    | _ -> assert false

  let delete_field (heap : t) ~(loc : value) ~(field : value) (solver : Batch.t)
    (pc : encoded_pct list) : (t * encoded_pct list) list =
    delete_field_aux heap loc field solver pc

  let to_string (_heap : t) : string = assert false
  (* let to_string (h : 'a t) (pp : 'a -> string) : string =
      "{ "
      ^ String.concat ~sep:", "
          (Hashtbl.fold h.map ~init:[] ~f:(fun ~key:n ~data:v acc ->
              Printf.sprintf "%s: %s" (Loc.str n) (S_object.str v pp) :: acc))
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

module V = Symbolic_value.M

module Make (O : Object_intf.S with type value = V.value) = struct
  type object_ = O.t

  type t =
    { parent : t option
    ; data : (Loc.t, object_) Hashtbl.t
    }

  type value = V.value

  let create () : t = { parent = None; data = Hashtbl.create 512 }
  let clone (m : t) : t = { parent = Some m; data = Hashtbl.create 16 }

  let insert ({ data = memory; _ } : t) (o : object_) : value =
    let loc = Loc.create () in
    Hashtbl.replace memory loc o;
    V.Val (Val.Loc loc)

  let remove (m : t) (l : Loc.t) : unit = Hashtbl.remove m.data l

  let set ({ data = memory; _ } : t) (key : Loc.t) (data : object_) : unit =
    Hashtbl.replace memory key data

  let rec get ({ parent; data } as h : t) (key : Loc.t) : object_ option =
    let open Syntax.Option in
    match Hashtbl.find_opt data key with
    | Some _ as result -> result
    | None ->
      let* parent in
      let+ obj = get parent key in
      let obj = O.clone obj in
      set h key obj;
      obj

  let has_field (h : t) (loc : Loc.t) (field : value) : value =
    Option.fold (get h loc)
      ~some:(fun o -> O.has_field o field)
      ~none:(V.Bool.const false)

  let set_field (h : t) (loc : Loc.t) ~(field : value) ~(data : value) : unit =
    Option.iter
      (fun o ->
        let o' = O.set o ~key:field ~data in
        set h loc o' )
      (get h loc)

  let get_field (h : t) (loc : Loc.t) (field : value) :
    (value * value list) list =
    let o = get h loc in
    Option.fold o ~none:[] ~some:(fun o -> O.get o field)

  let delete_field (h : t) (loc : Loc.t) (f : value) =
    let obj = get h loc in
    Option.iter
      (fun o ->
        let o' = O.delete o f in
        set h loc o' )
      obj

  let pp_hashtbl ~pp_sep pp_v fmt v =
    Format.pp_print_seq ~pp_sep pp_v fmt (Hashtbl.to_seq v)

  let rec pp fmt ({ data; parent } : t) =
    let open Format in
    let pp_sep fmt () = fprintf fmt ",@ " in
    let pp_v fmt (key, data) = fprintf fmt "%a: %a" Loc.pp key O.pp data in
    let pp_parent fmt p =
      pp_print_option (fun fmt h -> fprintf fmt "%a@ <-@ " pp h) fmt p
    in
    fprintf fmt "%a{ %a }" pp_parent parent (pp_hashtbl ~pp_sep pp_v) data

  let rec unfold_ite ~(accum : value) (e : value) : (value option * string) list
      =
    let open V in
    let open Operator in
    match e with
    | Val (Val.Loc x) | Val (Val.Symbol x) -> [ (Some accum, x) ]
    | TriOpt (ITE, c, Val (Val.Loc l), e) ->
      let accum' = BinOpt (LogicalAnd, accum, UnOpt (LogicalNot, c)) in
      let tl = unfold_ite ~accum:accum' e in
      (Some (BinOpt (LogicalAnd, accum, c)), l) :: tl
    | _ -> assert false

  let loc (e : value) : ((value option * string) list, string) Result.t =
    let open V in
    match e with
    | Val (Val.Loc l) -> Ok [ (None, l) ]
    | TriOpt (Operator.ITE, c, Val (Val.Loc l), v) ->
      Ok ((Some c, l) :: unfold_ite ~accum:(UnOpt (Operator.LogicalNot, c)) v)
    | _ ->
      Error (Format.asprintf "Value '%a' is not a loc expression" V.Pp.pp e)

  let pp_val (h : t) (e : value) : string =
    match e with
    | V.Val (Val.Loc l) -> (
      match get h l with
      | None -> l
      | Some o -> Format.asprintf "%s -> %a" l O.pp o )
    | _ -> Format.asprintf "%a" V.Pp.pp e
end

module M :
  Memory_intf.S with type value = V.value and type object_ = Symbolic_object.M.t =
  Make (Symbolic_object.M)

include M

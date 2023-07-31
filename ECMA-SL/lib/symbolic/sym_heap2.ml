open Core
module V = Sym_value.M

let ( let* ) o f = Option.bind o ~f

module Value_key : Hashtbl.Key with type t = V.value = struct
  type t = V.value

  let hash (e : t) = Hashtbl.hash e
  let t_of_sexp _ = assert false
  let sexp_of_t _ = assert false
  let compare (e1 : t) (e2 : t) = compare (Hashtbl.hash e1) (Hashtbl.hash e2)
end

module Object = struct
  module VMap = Map.Make (Value_key)

  type value = V.value

  type t =
    { fields : value VMap.t
    ; symbols : value VMap.t
    }

  let create () = { fields = VMap.empty; symbols = VMap.empty }
  let is_empty (o : t) : bool = VMap.(is_empty o.fields && is_empty o.symbols)

  let to_list (o : t) : (value * value) list =
    VMap.to_alist o.symbols @ VMap.to_alist o.fields

  let get_fields (o : t) : value list = VMap.(keys o.symbols @ keys o.fields)
  let eq v1 v2 = V.BinOpt (Operators.Eq, v1, v2)
  let ne v1 v2 = V.UnOpt (Operators.Not, eq v1 v2)
  let and_ v1 v2 = V.BinOpt (Operators.Log_And, v1, v2)
  let ite c v1 v2 = V.TriOpt (Operators.ITE, c, v1, v2)
  let undef = V.Val (Val.Symbol "undefined")
  let is_val = function V.Val _ -> true | _ -> false

  let has_field (o : t) (k : value) : value =
    if VMap.is_empty o.fields && VMap.is_empty o.symbols then V.Bool.const false
    else if is_val k then V.Bool.const (VMap.mem o.fields k)
    else
      let v0 =
        VMap.fold o.symbols ~init:(V.Bool.const false)
          ~f:(fun ~key ~data:_ accum ->
          ite (eq k key) (V.Bool.const true) accum )
      in
      VMap.fold o.fields ~init:v0 ~f:(fun ~key ~data:_ accum ->
        ite (eq k key) (V.Bool.const true) accum )

  let map_ite (m : value VMap.t) (k : value) (d : value) =
    VMap.mapi m ~f:(fun ~key ~data -> ite (eq k key) d data)

  let set (o : t) ~(key : value) ~(data : value) : t =
    if is_val key then { o with fields = VMap.set o.fields ~key ~data }
    else
      let fields = map_ite o.fields key data in
      let symbols0 = map_ite o.symbols key data in
      let symbols =
        match get_fields o with
        | [] -> VMap.set symbols0 ~key ~data
        | h :: t ->
          let old_d = Option.value (VMap.find symbols0 key) ~default:undef in
          let cond =
            List.fold t ~init:(ne key h) ~f:(fun accum a ->
              and_ (ne key a) accum )
          in
          VMap.set symbols0 ~key ~data:(ite cond data old_d)
      in
      { fields; symbols }

  let fold_ite ?(init = undef) (m : value VMap.t) (key : value) =
    if VMap.is_empty m then init
    else
      VMap.fold m ~init ~f:(fun ~key:key0 ~data accum ->
        ite (eq key key0) data accum )

  (* TODO: Make this return option explicitly *)
  (* FIXME: @174 *)
  let get (o : t) (key : value) : value =
    if is_val key then
      if VMap.mem o.fields key then VMap.find_exn o.fields key
      else fold_ite o.symbols key
    else
      let v0 = Option.value (VMap.find o.symbols key) ~default:undef in
      let v1 =
        VMap.fold o.symbols ~init:v0 ~f:(fun ~key:key0 ~data accum ->
          if V.equal key key0 then accum else ite (eq key key0) data accum )
      in
      fold_ite ~init:v1 o.fields key

  let delete (o : t) (key : value) : t =
    if is_val key then { o with fields = VMap.remove o.fields key }
    else assert false

  let to_string (o : t) : string =
    let fold_str m =
      VMap.fold m ~init:"" ~f:(fun ~key ~data accum ->
        let k = V.Pp.pp key
        and d = V.Pp.pp data in
        Format.sprintf "%s \"%s\": %s," accum k d )
    in
    let fields = fold_str o.fields
    and symbols = fold_str o.symbols in
    sprintf "{%s%s }" fields symbols

  let to_json : t -> string = to_string
end

module Heap = struct
  type object_ = Object.t
  type t = (Loc.t, object_) Hashtbl.t
  type value = V.value

  let create () : t = Hashtbl.create (module String)
  let clone (h : t) : t = Hashtbl.copy h

  let insert (h : t) (o : object_) : value =
    let loc = Loc.newloc () in
    Hashtbl.set h ~key:loc ~data:o;
    V.Val (Val.Loc loc)

  let remove (h : t) (l : Loc.t) : unit = Hashtbl.remove h l

  let set (h : t) (key : Loc.t) (data : object_) : unit =
    Hashtbl.set h ~key ~data

  let get (h : t) (key : Loc.t) : object_ option = Hashtbl.find h key

  let has_field (h : t) (loc : Loc.t) (field : value) : value =
    Option.value_map (get h loc) ~default:(V.Bool.const false) ~f:(fun o ->
      Object.has_field o field )

  let set_field (h : t) (loc : Loc.t) ~(field : value) ~(data : value) : unit =
    Option.iter (get h loc) ~f:(fun o ->
      let o' = Object.set o ~key:field ~data in
      set h loc o' )

  let get_field (h : t) (loc : Loc.t) (field : value) : value option =
    let* o = get h loc in
    Some (Object.get o field)

  let delete_field (h : t) (loc : Loc.t) (f : value) =
    let obj = get h loc in
    Option.iter obj ~f:(fun o ->
      let o' = Object.delete o f in
      set h loc o' )

  let to_string (h : t) : string =
    let map =
      Hashtbl.fold h ~init:[] ~f:(fun ~key ~data accum ->
        sprintf "%s: %s" (Loc.str key) (Object.to_string data) :: accum )
    in
    sprintf "{ %s }" (String.concat ~sep:", " map)

  let rec unfold_ite ~(accum : value) (e : value) : (value option * string) list
      =
    let open V in
    let open Operators in
    match e with
    | Val (Val.Loc x) | Val (Val.Symbol x) -> [ (Some accum, x) ]
    | TriOpt (ITE, c, Val (Val.Loc l), e) ->
      let accum' = BinOpt (Log_And, accum, UnOpt (Not, c)) in
      let tl = unfold_ite ~accum:accum' e in
      (Some (BinOpt (Log_And, accum, c)), l) :: tl
    | _ -> assert false

  let loc (e : value) : ((value option * string) list, string) Result.t =
    let open V in
    match e with
    | Val (Val.Loc l) -> Ok [ (None, l) ]
    | TriOpt (Operators.ITE, c, Val (Val.Loc l), v) ->
      Ok ((Some c, l) :: unfold_ite ~accum:(UnOpt (Operators.Not, c)) v)
    | _ -> Error (sprintf "Value '%s' is not a loc expression" (V.Pp.pp e))

  (* let to_string_with_glob (h : 'a t) (pp : 'a -> string) : string = *)
  (*   let glob = *)
  (*     Hashtbl.fold h.map ~init:None ~f:(fun ~key:_ ~data:obj acc -> *)
  (*         match acc with *)
  (*         | Some _ -> acc *)
  (*         (1* Keep this in sync with Compiler.ml function *1) *)
  (*         (1* "compile_gvar" and "compile_glob_assign" *1) *)
  (*         | None -> Object.get obj Common.global_var_compiled) *)
  (*   in *)
  (*   match glob with *)
  (*   | Some l -> *)
  (*       Printf.sprintf "{ \"heap\": %s, \"global\": %s }" (to_string h pp) *)
  (*         (Val.str l) *)
  (*   | None -> *)
  (*       raise *)
  (*         (Failure *)
  (* "Couldn't find the Object that contains only one property, named \ *)
     (*             \"global\".") *)
end

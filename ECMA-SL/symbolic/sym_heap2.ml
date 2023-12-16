module V = Sym_value.M

let ( let* ) o f = match o with None -> None | Some v -> f v
let eq v1 v2 = V.BinOpt (Operators.Eq, v1, v2)
let ne v1 v2 = V.UnOpt (Operators.LogicalNot, eq v1 v2)
let ite c v1 v2 = V.TriOpt (Operators.ITE, c, v1, v2)
let undef = V.Val (Val.Symbol "undefined")
let is_val = function V.Val _ -> true | _ -> false

module Value_key = struct
  type t = V.value

  let hash (e : t) = Hashtbl.hash e
  let t_of_sexp _ = assert false
  let sexp_of_t _ = assert false
  let compare (e1 : t) (e2 : t) = compare (Hashtbl.hash e1) (Hashtbl.hash e2)
end

module Object : sig
  type t
  type value = V.value

  val create : unit -> t
  val is_empty : t -> bool
  val to_list : t -> (value * value) list
  val get_fields : t -> value list
  val has_field : t -> value -> value
  val set : t -> key:value -> data:value -> t
  val get : t -> value -> (value * value list) list
  val delete : t -> value -> t
  val to_string : t -> string
  val to_json : t -> string
end = struct
  module VMap = Map.Make (Value_key)

  type value = V.value

  type t =
    { fields : value VMap.t
    ; symbols : value VMap.t
    }

  let create () = { fields = VMap.empty; symbols = VMap.empty }
  let is_empty o = VMap.(is_empty o.fields && is_empty o.symbols)
  let to_list o = VMap.to_list o.symbols @ VMap.to_list o.fields

  let get_fields o =
    let symbols = VMap.fold (fun key _ acc -> key :: acc) o.symbols [] in
    VMap.fold (fun key _ acc -> key :: acc) o.fields symbols

  let has_field o k =
    if VMap.is_empty o.fields && VMap.is_empty o.symbols then V.Bool.const false
    else
      match k with
      | V.Val _ as v -> V.Bool.const (VMap.mem v o.fields)
      | _ ->
        let r0 =
          VMap.fold
            (fun key _ acc -> ite (eq k key) (V.Bool.const true) acc)
            o.symbols (V.Bool.const false)
        in
        VMap.fold
          (fun key _ acc -> ite (eq k key) (V.Bool.const true) acc)
          o.fields r0

  let map_ite (m : value VMap.t) ~(key : value) ~(data : value) =
    VMap.mapi
      (fun key0 data0 ->
        if V.equal key key0 then data0 else ite (eq key key0) data data0 )
      m

  let set o ~key ~data =
    match key with
    | V.Val v -> { o with fields = VMap.add (V.Val v) data o.fields }
    | _ ->
      { fields = map_ite o.fields ~key ~data
      ; symbols = map_ite o.symbols ~key ~data |> VMap.add key data
      }

  let fold_eq (m : value VMap.t) (key0 : value) : (value * value) list =
    VMap.fold
      (fun key data acc ->
        if V.equal key0 key then acc else (data, eq key0 key) :: acc )
      m []

  (* FIXME: @174 *)
  let get { fields; symbols } key =
    match key with
    | V.Val _ -> (
      match VMap.find_opt key fields with
      | Some v -> [ (v, []) ]
      | None -> (
        match fold_eq symbols key with
        | [] -> []
        | [ (v, cond) ] -> [ (v, [ cond ]); (undef, [ V.Bool.not_ cond ]) ]
        | (v0, cond0) :: tl ->
          let v, neg_conds =
            List.fold_left
              (fun (acc, neg_conds) (v1, cond1) ->
                (ite cond1 v1 acc, V.Bool.not_ cond1 :: neg_conds) )
              (v0, [ V.Bool.not_ cond0 ])
              tl
          in
          [ (v, []); (undef, neg_conds) ] ) )
    | _ -> (
      match VMap.find_opt key symbols with
      | Some v -> [ (v, []) ]
      | None -> (
        match fold_eq fields key with
        | [] -> []
        | [ (v, cond) ] -> [ (v, [ cond ]); (undef, [ V.Bool.not_ cond ]) ]
        | (v0, cond0) :: tl ->
          let v, neg_conds =
            List.fold_left
              (fun (acc, neg_conds) (v1, cond1) ->
                (ite cond1 v1 acc, V.Bool.not_ cond1 :: neg_conds) )
              (v0, [ V.Bool.not_ cond0 ])
              tl
          in
          [ (v, []); (undef, neg_conds) ] ) )

  let delete o key =
    match key with
    | V.Val _ -> { o with fields = VMap.remove key o.fields }
    | _ -> assert false

  let to_string { fields; symbols } =
    let fold_str map =
      VMap.fold
        (fun key data acc ->
          Format.sprintf "%s \"%s\": %s," acc (V.Pp.pp key) (V.Pp.pp data) )
        map ""
    in
    Format.sprintf "{%s%s }" (fold_str fields) (fold_str symbols)

  let to_json = to_string
end

module Heap = struct
  type object_ = Object.t
  type t = (Loc.t, object_) Hashtbl.t
  type value = V.value

  let create () : t = Hashtbl.create 512
  let clone (h : t) : t = Hashtbl.copy h

  let insert (h : t) (o : object_) : value =
    let loc = Loc.newloc () in
    Hashtbl.replace h loc o;
    V.Val (Val.Loc loc)

  let remove (h : t) (l : Loc.t) : unit = Hashtbl.remove h l

  let set (h : t) (key : Loc.t) (data : object_) : unit =
    Hashtbl.replace h key data

  let get (h : t) (key : Loc.t) : object_ option = Hashtbl.find_opt h key

  let has_field (h : t) (loc : Loc.t) (field : value) : value =
    Option.map_default
      (fun o -> Object.has_field o field)
      (V.Bool.const false) (get h loc)

  let set_field (h : t) (loc : Loc.t) ~(field : value) ~(data : value) : unit =
    Option.may
      (fun o ->
        let o' = Object.set o ~key:field ~data in
        set h loc o' )
      (get h loc)

  let get_field (h : t) (loc : Loc.t) (field : value) :
    (value * value list) list =
    let o = get h loc in
    Option.map_default (fun o -> Object.get o field) [] o

  let delete_field (h : t) (loc : Loc.t) (f : value) =
    let obj = get h loc in
    Option.may
      (fun o ->
        let o' = Object.delete o f in
        set h loc o' )
      obj

  let to_string (h : t) : string =
    let map =
      Hashtbl.fold
        (fun key data acc ->
          Format.sprintf "%s: %s" (Loc.str key) (Object.to_string data) :: acc
          )
        h []
    in
    Format.sprintf "{ %s }" (String.concat ", " map)

  let rec unfold_ite ~(accum : value) (e : value) : (value option * string) list
      =
    let open V in
    let open Operators in
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
    | TriOpt (Operators.ITE, c, Val (Val.Loc l), v) ->
      Ok ((Some c, l) :: unfold_ite ~accum:(UnOpt (Operators.LogicalNot, c)) v)
    | _ ->
      Error (Format.sprintf "Value '%s' is not a loc expression" (V.Pp.pp e))

  let pp (h : t) (e : value) : string =
    match e with
    | V.Val (Val.Loc l) -> (
      match get h l with
      | None -> l
      | Some o -> Format.sprintf "%s -> %s" l (Object.to_string o) )
    | _ -> V.Pp.pp e
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

open EslBase
open EslSyntax
open Smtml
module V = Symbolic_value.M
module E = Smtml.Expr

module Make (O : Object_intf.S with type value = V.value) = struct
  type object_ = O.t

  type t =
    { parent : t option
    ; data : object_ Loc.Tbl.t
    }

  type value = V.value

  let create () : t = { parent = None; data = Loc.Tbl.create 512 }
  let clone (m : t) : t = { parent = Some m; data = Loc.Tbl.create 16 }

  let insert ({ data = memory; _ } : t) (o : object_) : value =
    let loc = Loc.create () in
    Loc.Tbl.replace memory loc o;
    E.(value (App (`Op "loc", [ Int loc ])))

  let remove (m : t) (l : Loc.t) : unit = Loc.Tbl.remove m.data l

  let set ({ data = memory; _ } : t) (key : Loc.t) (data : object_) : unit =
    Loc.Tbl.replace memory key data

  let find memory l =
    let open Smtml_prelude.Option in
    let rec aux { parent; data } l from_parent =
      match Loc.Tbl.find_opt data l with
      | Some o -> Some (o, from_parent)
      | None ->
        let* parent in
        aux parent l true
    in
    aux memory l false

  let get memory l =
    let open Smtml_prelude.Option in
    let+ (obj, from_parent) = find memory l in
    match from_parent with
    | false -> obj
    | true ->
      let obj = O.clone obj in
      set memory l obj;
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

  let rec pp ppf ({ data; parent } : t) =
    let open Fmt in
    let pp_v ppf (key, data) = fmt ppf "%a: %a" Loc.pp key O.pp data in
    let pp_parent ppf v = pp_opt (fun ppf h -> fmt ppf "%a@ <-@ " pp h) ppf v in
    fmt ppf "%a{ %a }" pp_parent parent (Loc.Tbl.pp !>", " pp_v) data

  let rec unfold_ite ~(accum : value) (e : value) : (value option * int) list =
    match E.view e with
    | Val (App (`Op "loc", [ Int x ])) -> [ (Some accum, x) ]
    (* TODO:x | Val (Val.Symbol _x) -> [ (Some accum, ~-1) ] *)
    | Triop (_, Ty.Ite, c, a, e) -> (
      match E.view a with
      | Val (App (`Op "loc", [ Int l ])) ->
        let accum' =
          E.(binop Ty.Ty_bool Ty.And accum (unop Ty.Ty_bool Ty.Not c))
        in
        let tl = unfold_ite ~accum:accum' e in
        (Some E.(binop Ty.Ty_bool Ty.And accum c), l) :: tl
      | _ -> assert false )
    | _ -> assert false

  let loc (e : value) : ((value option * int) list, string) Result.t =
    match E.view e with
    | Val (App (`Op "symbol", [ Str "undefined" ])) ->
      (* We're in an unsat path *)
      Ok []
    | Val (App (`Op "loc", [ Int l ])) -> Ok [ (None, l) ]
    | Triop (_, Ty.Ite, c, a, v) -> (
      match E.view a with
      | Val (App (`Op "loc", [ Int l ])) ->
        Ok ((Some c, l) :: unfold_ite ~accum:E.(unop Ty.Ty_bool Ty.Not c) v)
      | _ -> Error (Fmt.str "Value '%a' is not a loc expression" E.pp e) )
    | _ ->
      Fmt.eprintf "Value '%a' is not a loc expression" V.pp e;
      Ok []

  let pp_val (h : t) (e : value) : string =
    match E.view e with
    | Val (App (`Op "loc", [ Int l ])) -> (
      match get h l with
      | None -> Loc.str l
      | Some o -> Fmt.str "%a -> %a" Loc.pp l O.pp o )
    | _ -> Fmt.str "%a" V.pp e
end

module M :
  Memory_intf.S with type value = V.value and type object_ = Symbolic_object.M.t =
  Make (Symbolic_object.M)

include M

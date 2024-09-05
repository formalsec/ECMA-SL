open EslBase
open EslSyntax

type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : 'a obj Loc.Tbl.t
  }

let default () : 'a t =
  { parent = None; map = Loc.Tbl.create !Base.default_hashtbl_sz }
[@@inline]

let create () : 'a t = default () [@@inline]
let length (heap : 'a t) : int = Loc.Tbl.length heap.map [@@inline]

let extend (heap : 'a t) : 'a t = { (create ()) with parent = Some heap }
[@@inline]

let shrunk (heap : 'a t) : 'a t = Option.value ~default:heap heap.parent
[@@inline]

let rec get (heap : 'a t) (loc : Loc.t) : 'a obj option =
  (* Hack: required to call the get function recursively *)
  let get' = get in
  match Loc.Tbl.find_opt heap.map loc with
  | Some _ as obj -> obj
  | None ->
    let open Smtml.Syntax.Option in
    let* parent = heap.parent in
    let+ obj = get' parent loc in
    let obj' = Object.clone obj in
    Loc.Tbl.replace heap.map loc obj';
    obj'

let set (heap : 'a t) (loc : Loc.t) (obj : 'a obj) : unit =
  Loc.Tbl.replace heap.map loc obj
[@@inline]

let pp_map (pp_v : Fmt.t -> 'a obj -> unit) (ppf : Fmt.t)
  (map : 'a obj Loc.Tbl.t) : unit =
  let pp_bind ppf (loc, obj) = Fmt.fmt ppf "%a: %a" Loc.pp loc pp_v obj in
  if Loc.Tbl.length map == 0 then Fmt.pp_str ppf "{}"
  else Fmt.fmt ppf "{ %a }" Fmt.(Loc.Tbl.pp !>", " pp_bind) map

let rec pp (pp_v : Fmt.t -> 'a obj -> unit) (ppf : Fmt.t) (heap : 'a t) : unit =
  let pp_parent ppf heap = Fmt.fmt ppf "%a <- " (pp pp_v) heap in
  Fmt.fmt ppf "%a%a" (Fmt.pp_opt pp_parent) heap.parent (pp_map pp_v) heap.map

let str (pp_v : Fmt.t -> 'a obj -> unit) (heap : 'a t) : string =
  Fmt.str "%a" (pp pp_v) heap
[@@inline]

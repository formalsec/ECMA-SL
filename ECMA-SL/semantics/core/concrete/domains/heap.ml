open EslBase
open EslSyntax

type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : 'a obj Loc.Tbl.t
  }

let default () : 'a t =
  { parent = None; map = Loc.Tbl.create !Base.default_hashtbl_sz }

let create () : 'a t =
  { parent = None; map = Loc.Tbl.create !Base.default_hashtbl_sz }

let extend (heap : 'a t) : 'a t = { (create ()) with parent = Some heap }
let shrunk (heap : 'a t) : 'a t = Option.value ~default:heap heap.parent

let rec get_opt (heap : 'a t) (l : Loc.t) : 'a obj option =
  let open Syntax.Option in
  match Loc.Tbl.find_opt heap.map l with
  | Some _ as obj -> obj
  | None ->
    let* parent = heap.parent in
    let+ obj = get_opt parent l in
    let obj' = Object.clone obj in
    Loc.Tbl.replace heap.map l obj';
    obj'

let get (heap : 'a t) (l : Loc.t) : ('a obj, string) Result.t =
  match get_opt heap l with
  | None -> Error (Fmt.asprintf "Cannot find location '%a'." Loc.pp l)
  | Some obj -> Ok obj

let set (heap : 'a t) (l : Loc.t) (obj : 'a obj) : unit =
  Loc.Tbl.replace heap.map l obj

let rec pp (pp_obj : Fmt.t -> 'a obj -> unit) (fmt : Fmt.t) (heap : 'a t) : unit
    =
  let open Fmt in
  let pp_binding fmt (loc, obj) = fprintf fmt "%a: %a" Loc.pp loc pp_obj obj in
  let pp_map fmt map =
    if Loc.Tbl.length map = 0 then pp_str fmt "{}"
    else fprintf fmt "{ %a }" (Loc.Tbl.pp ", " pp_binding) map
  in
  let pp_parent fmt heap = fprintf fmt "%a <- " (pp pp_obj) heap in
  fprintf fmt "%a%a" (pp_opt pp_parent) heap.parent pp_map heap.map

let rec pp_tabular (pp_obj : Fmt.t -> 'a obj -> unit) (fmt : Fmt.t) (heap : 'a t)
  : unit =
  let open Fmt in
  let lengths =
    Loc.Tbl.to_seq_keys heap.map
    |> Seq.map (fun loc -> String.length (Loc.str loc))
  in
  let max = Seq.fold_left Int.max 0 lengths in
  let indent x = String.make (max - String.length (Loc.str x)) ' ' in
  let pp_bind fmt (x, v) =
    fprintf fmt "%s%a  <-  %a" (indent x) Loc.pp x pp_obj v
  in
  let pp_parent fmt heap = fprintf fmt "%a\n\n^\n\n" (pp_tabular pp_obj) heap in
  fprintf fmt "%a%a" (pp_opt pp_parent) heap.parent (Loc.Tbl.pp "\n" pp_bind)
    heap.map

let str ?(tabular : bool = false) (pp_obj : Fmt.t -> 'a obj -> unit)
  (heap : 'a t) : string =
  Fmt.asprintf "%a" (if tabular then pp_tabular pp_obj else pp pp_obj) heap

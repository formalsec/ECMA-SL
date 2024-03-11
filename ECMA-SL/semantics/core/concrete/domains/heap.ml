open EslCore
open EslSyntax

type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : (Loc.t, 'a obj) Hashtbl.t
  }

let create () : 'a t =
  { parent = None; map = Hashtbl.create !Base.default_hashtbl_sz }

let extend (heap : 'a t) : 'a t = { (create ()) with parent = Some heap }
let shrunk (heap : 'a t) : 'a t = Option.value ~default:heap heap.parent

let rec get_opt (heap : 'a t) (l : Loc.t) : 'a obj option =
  let open Syntax.Option in
  match Hashtbl.find_opt heap.map l with
  | Some _ as obj -> obj
  | None ->
    let* parent = heap.parent in
    let+ obj = get_opt parent l in
    let obj' = Object.clone obj in
    Hashtbl.replace heap.map l obj';
    obj'

let get (heap : 'a t) (l : Loc.t) : ('a obj, string) Result.t =
  match get_opt heap l with
  | None -> Error (Fmt.sprintf "Cannot find location '%s'." l)
  | Some obj -> Ok obj

let set (heap : 'a t) (l : Loc.t) (obj : 'a obj) : unit =
  Hashtbl.replace heap.map l obj

let rec pp (pp_obj : Fmt.t -> 'a obj -> unit) (fmt : Fmt.t) (heap : 'a t) : unit
    =
  let open Fmt in
  let pp_binding fmt (x, obj) = fprintf fmt "%s: %a" x pp_obj obj in
  let pp_map fmt map =
    if Hashtbl.length map = 0 then pp_str fmt "{}"
    else fprintf fmt "{ %a }" (pp_hashtbl ", " pp_binding) map
  in
  let pp_parent fmt heap = fprintf fmt "%a <- " (pp pp_obj) heap in
  fprintf fmt "%a%a" (pp_opt pp_parent) heap.parent pp_map heap.map

let rec pp_tabular (pp_obj : Fmt.t -> 'a obj -> unit) (fmt : Fmt.t) (heap : 'a t)
  : unit =
  let open Fmt in
  let lengths = Hashtbl.to_seq_keys heap.map |> Seq.map String.length in
  let max = Seq.fold_left Int.max 0 lengths in
  let indent x = String.make (max - String.length x) ' ' in
  let pp_bind fmt (x, v) = fprintf fmt "%s%s  <-  %a" (indent x) x pp_obj v in
  let pp_parent fmt heap = fprintf fmt "%a\n\n^\n\n" (pp_tabular pp_obj) heap in
  fprintf fmt "%a%a" (pp_opt pp_parent) heap.parent (pp_hashtbl "\n" pp_bind)
    heap.map

let str ?(tabular : bool = false) (pp_obj : Fmt.t -> 'a obj -> unit)
  (heap : 'a t) : string =
  Fmt.asprintf "%a" (if tabular then pp_tabular pp_obj else pp pp_obj) heap

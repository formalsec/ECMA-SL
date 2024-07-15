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

let length (heap : 'a t) : int = Loc.Tbl.length heap.map
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
  | None -> Error (Fmt.str "Cannot find location '%a'." Loc.pp l)
  | Some obj -> Ok obj

let set (heap : 'a t) (l : Loc.t) (obj : 'a obj) : unit =
  Loc.Tbl.replace heap.map l obj

let rec pp (pp_obj : Fmt.t -> 'a obj -> unit) (ppf : Fmt.t) (heap : 'a t) : unit
    =
  let open Fmt in
  let pp_binding ppf (loc, obj) = fmt ppf "%a: %a" Loc.pp loc pp_obj obj in
  let pp_map ppf map =
    if Loc.Tbl.length map = 0 then pp_str ppf "{}"
    else fmt ppf "{ %a }" (Loc.Tbl.pp !>", " pp_binding) map
  in
  let pp_parent ppf heap = fmt ppf "%a <- " (pp pp_obj) heap in
  fmt ppf "%a%a" (pp_opt pp_parent) heap.parent pp_map heap.map

let rec pp_tabular (pp_obj : Fmt.t -> 'a obj -> unit) (ppf : Fmt.t) (heap : 'a t)
  : unit =
  let open Fmt in
  let lengths =
    Loc.Tbl.to_seq_keys heap.map
    |> Seq.map (fun loc -> String.length (Loc.str loc))
  in
  let max = Seq.fold_left Int.max 0 lengths in
  let indent x = String.make (max - String.length (Loc.str x)) ' ' in
  let pp_bind ppf (x, v) =
    fmt ppf "%s%a  <-  %a" (indent x) Loc.pp x pp_obj v
  in
  let pp_parent ppf heap = fmt ppf "%a\n\n^\n\n" (pp_tabular pp_obj) heap in
  fmt ppf "%a%a" (pp_opt pp_parent) heap.parent
    (Loc.Tbl.pp !>"\n" pp_bind)
    heap.map

let str ?(tabular : bool = false) (pp_obj : Fmt.t -> 'a obj -> unit)
  (heap : 'a t) : string =
  Fmt.str "%a" (if tabular then pp_tabular pp_obj else pp pp_obj) heap

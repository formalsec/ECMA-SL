type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : (Loc.t, 'a obj) Hashtbl.t
  }

let create () : 'a t =
  { parent = None; map = Hashtbl.create !Config.default_hashtbl_sz }

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

let pp (pp_obj : Fmt.t -> 'a obj -> unit) (fmt : Fmt.t) (heap : 'a t) : unit =
  let open Fmt in
  let pp_binding fmt (x, obj) = fprintf fmt "%s: %a" x pp_obj obj in
  if Hashtbl.length heap.map = 0 then pp_str fmt "{}"
  else fprintf fmt "{ %a }" (pp_hashtbl ", " pp_binding) heap.map

let pp_tabular (pp_obj : Fmt.t -> 'a obj -> unit) (fmt : Fmt.t) (heap : 'a t) :
  unit =
  let open Fmt in
  let lengths = Hashtbl.to_seq_keys heap.map |> Seq.map String.length in
  let max = Seq.fold_left Int.max 0 lengths in
  let indent x = String.make (max - String.length x) ' ' in
  let pp_bind fmt (x, v) = fprintf fmt "%s%s  <-  %a" (indent x) x pp_obj v in
  fprintf fmt "%a" (pp_hashtbl "\n" pp_bind) heap.map

let rec pp_level ?(tabular : bool = false) (pp_obj : Fmt.t -> 'a obj -> unit)
  (fmt : Fmt.t) (heap : 'a t) : unit =
  let sep = if tabular then "\n^^^\n" else " <- " in
  let pp_parent fmt = function
    | Some p -> Fmt.fprintf fmt "%a%s" (pp_level ~tabular pp_obj) p sep
    | _ -> ()
  in
  let pp_level = if tabular then pp_tabular pp_obj else pp pp_obj in
  Fmt.fprintf fmt "%a%a" pp_parent heap.parent pp_level heap

let str ?(tabular : bool = false) (pp_obj : Fmt.t -> 'a obj -> unit)
  (heap : 'a t) : string =
  Fmt.asprintf "%a" (pp_level ~tabular pp_obj) heap

type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : (Loc.t, 'a obj) Hashtbl.t
  }

let create () : 'a t =
  { parent = None; map = Hashtbl.create !Config.default_hashtbl_sz }

let clone (heap : 'a t) : 'a t =
  { parent = Some heap; map = Hashtbl.create !Config.default_hashtbl_sz }

let set (heap : 'a t) (l : Loc.t) (obj : 'a obj) : unit =
  Hashtbl.replace heap.map l obj

let rec get_opt (heap : 'a t) (l : Loc.t) : 'a obj option =
  match Hashtbl.find_opt heap.map l with
  | Some _ as obj -> obj
  | None ->
    let open Syntax.Option in
    let* parent = heap.parent in
    let+ obj = get_opt parent l in
    let obj' = Object.clone obj in
    set heap l obj';
    obj'

let get (heap : 'a t) (l : Loc.t) : ('a obj, string) Result.t =
  match get_opt heap l with
  | None -> Error (Fmt.sprintf "Cannot find location '%s'." l)
  | Some obj -> Ok obj

let get_field_opt (heap : 'a t) (l : Loc.t) (fn : string) : 'a option =
  let open Syntax.Option in
  let* obj = get_opt heap l in
  Object.get obj fn

let get_field (heap : 'a t) (l : Loc.t) (fn : string) : ('a, string) Result.t =
  match get_field_opt heap l fn with
  | None -> Error (Fmt.sprintf "Cannot find field '%s' in location '%s'." fn l)
  | Some fld -> Ok fld

let insert (heap : 'a t) (obj : 'a obj) : Loc.t =
  let l = Loc.create () in
  Hashtbl.replace heap.map l obj;
  l

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

let str ?(tabular : bool = false) (pp_obj : Fmt.t -> 'a obj -> unit)
  (heap : 'a t) : string =
  Fmt.asprintf "%a" (if tabular then pp_tabular pp_obj else pp pp_obj) heap

type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : (Loc.t, 'a obj) Hashtbl.t
  }

type 'a pp_fmt = Fmt.formatter -> 'a obj -> unit

let create () : 'a t =
  { parent = None; map = Hashtbl.create !Config.default_hashtbl_sz }

let clone (heap : 'a t) : 'a t =
  { parent = Some heap; map = Hashtbl.create !Config.default_hashtbl_sz }

let insert (heap : 'a t) (obj : 'a obj) : Loc.t =
  let l = Loc.create () in
  Hashtbl.add heap.map l obj;
  l

let set (heap : 'a t) (l : Loc.t) (obj : 'a obj) : unit =
  Hashtbl.replace heap.map l obj

let rec get_opt (heap : 'a t) (l : Loc.t) : 'a obj option =
  let open Syntax.Option in
  match Hashtbl.find_opt heap.map l with
  | Some _ as obj -> obj
  | None ->
    let* parent = heap.parent in
    let+ obj = get_opt parent l in
    let obj' = Object.clone obj in
    set heap l obj';
    obj'

let get (heap : 'a t) (l : Loc.t) : ('a obj, string) Result.t =
  match get_opt heap l with
  | Some obj -> Ok obj
  | None -> Error (Fmt.sprintf "Cannot find location '%s'." l)

let get_field_opt (heap : 'a t) (l : Loc.t) (fn : string) : 'a option =
  let open Syntax.Option in
  let* obj = get_opt heap l in
  Object.get obj fn

let get_field (heap : 'a t) (l : Loc.t) (fn : string) : ('a, string) Result.t =
  match get_field_opt heap l fn with
  | Some fld -> Ok fld
  | None -> Error (Fmt.sprintf "Cannot find field '%s' in location '%s'." fn l)

let pp_entry (pp_binding : Fmt.formatter -> Loc.t * 'a obj -> unit)
  (sep : string) (fmt : Fmt.formatter) (heap : 'a t) : unit =
  let open Fmt in
  let pp_sep fmt () = pp_print_string fmt sep in
  let pp_seq pp fmt lst = pp_print_seq ~pp_sep pp fmt lst in
  fprintf fmt "%a" (pp_seq pp_binding) (Hashtbl.to_seq heap.map)

let pp (pp_obj : 'a pp_fmt) (fmt : Fmt.formatter) (heap : 'a t) : unit =
  let open Fmt in
  let pp_binding fmt (x, obj) = fprintf fmt "%s: %a" x pp_obj obj in
  fprintf fmt "{ %a }" (pp_entry pp_binding ", ") heap

let pp_tabular (pp_obj : 'a pp_fmt) (fmt : Fmt.formatter) (heap : 'a t) : unit =
  let open Fmt in
  let lengths = Hashtbl.to_seq_keys heap.map |> Seq.map String.length in
  let max = Seq.fold_left Int.max 0 lengths in
  let sep x = String.make (max - String.length x) ' ' in
  let pp_binding fmt (x, v) = fprintf fmt "%s%s  <-  %a" (sep x) x pp_obj v in
  pp_entry pp_binding "\n" fmt heap

let str ?(tabular : bool = false) (pp_obj : 'a pp_fmt) (heap : 'a t) : string =
  if tabular then Fmt.asprintf "%a" (pp_tabular pp_obj) heap
  else Fmt.asprintf "%a" (pp pp_obj) heap

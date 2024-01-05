type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : (Loc.t, 'a obj) Hashtbl.t
  }

type 'a pp_fmt = Format.formatter -> 'a obj -> unit

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
  | None -> Error (Format.sprintf "Cannot find lation '%s'." l)

let get_field_opt (heap : 'a t) (l : Loc.t) (fn : string) : 'a option =
  let open Syntax.Option in
  let* obj = get_opt heap l in
  Object.get obj fn

let pp (pp_binding : Format.formatter -> Loc.t * 'a obj -> unit) (sep : string)
  (fmt : Format.formatter) (heap : 'a t) : unit =
  let open Format in
  let pp_sep fmt () = pp_print_string fmt sep in
  let pp_seq pp fmt lst = pp_print_seq ~pp_sep pp fmt lst in
  fprintf fmt "%a" (pp_seq pp_binding) (Hashtbl.to_seq heap.map)

let pp_inline (pp_obj : 'a pp_fmt) (fmt : Format.formatter) (heap : 'a t) : unit
    =
  let open Format in
  let pp_bind fmt (x, obj) = fprintf fmt "%s: %a" x pp_obj obj in
  fprintf fmt "{ %a }" (pp pp_bind ", ") heap

let pp_table (pp_obj : 'a pp_fmt) (fmt : Format.formatter) (heap : 'a t) : unit
    =
  let open Format in
  let max_f acc n = if n > acc then n else acc in
  let lengths = Hashtbl.to_seq_keys heap.map |> Seq.map String.length in
  let max = Seq.fold_left max_f 0 lengths in
  let var_sep x = String.make (max - String.length x) ' ' in
  let pp_bind fmt (x, v) = fprintf fmt "%s%s | %a\n" (var_sep x) x pp_obj v in
  pp pp_bind "" fmt heap

let str ?(tabular : bool = true) (pp_obj : 'a pp_fmt) (heap : 'a t) : string =
  if tabular then Format.asprintf "%a" (pp_table pp_obj) heap
  else Format.asprintf "%a" (pp_inline pp_obj) heap

(* let str (pp_val : 'a Object.pp_fmt) (heap : 'a t) : string =
   let _str_loc l = Loc.str l in
   let _str_obj o = Object.str pp_val o in
   let _str_binding l o = Printf.sprintf "%s: %s" (_str_loc l) (_str_obj o) in
   let _str_heap_f l o acc = _str_binding l o :: acc in
   let heap_str = Hashtbl.fold _str_heap_f heap.map [] |> String.concat ", " in
   "{ " ^ heap_str ^ " }" *)

include Format

type t = Format.formatter

let pp_int (ppf : t) (i : int) : unit = pp_print_int ppf i
let pp_float (ppf : t) (f : float) : unit = pp_print_float ppf f
let pp_char (ppf : t) (c : char) : unit = pp_print_char ppf c
let pp_str (ppf : t) (s : string) : unit = pp_print_string ppf s
let pp_bool (ppf : t) (b : bool) : unit = pp_print_bool ppf b

let pp_iter (pp_sep : t -> unit -> unit) (iter : ('a -> unit) -> 'b -> unit)
  (pp_el : t -> 'a -> unit) (ppf : t) (el : 'b) : unit =
  let is_first = ref true in
  let pp_el' el =
    if !is_first then is_first := false else pp_sep ppf ();
    pp_el ppf el
  in
  iter pp_el' el

let pp_hashtbl (sep : string) (pp_el : t -> 'a * 'b -> unit) (ppf : t)
  (htbl : ('a, 'b) Hashtbl.t) =
  let hashtbl_iter f tbl = Hashtbl.iter (fun a b -> f (a, b)) tbl in
  pp_iter (fun ppf () -> pp_str ppf sep) hashtbl_iter pp_el ppf htbl

let pp_hashtbl2 ~(pp_sep : t -> unit -> unit) (pp_el : t -> 'a * 'b -> unit)
  (ppf : t) (v : ('a, 'b) Hashtbl.t) =
  let iter f tbl =
    let tbl =
      Hashtbl.to_seq tbl
      |> List.of_seq
      |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
    in
    List.iter f tbl
  in
  pp_iter pp_sep iter pp_el ppf v

let pp_arr (sep : string) (pp_el : t -> 'a -> unit) (ppf : t) (arr : 'a array) =
  pp_iter (fun ppf () -> pp_str ppf sep) Array.iter pp_el ppf arr

let pp_lst (sep : string) (pp_el : t -> 'a -> unit) (ppf : t) (lst : 'a list) =
  pp_print_list ~pp_sep:(fun ppf () -> pp_str ppf sep) pp_el ppf lst

let pp_seq (sep : string) (pp_el : t -> 'a -> unit) (ppf : t) (seq : 'a Seq.t) =
  pp_print_seq ~pp_sep:(fun ppf () -> pp_str ppf sep) pp_el ppf seq

let pp_opt (pp_el : t -> 'a -> unit) (ppf : t) (el : 'a option) =
  pp_print_option pp_el ppf el

let pp_cond (cond : bool) (pp_el : t -> 'a -> unit) (ppf : t) (el : 'a) =
  if cond then pp_el ppf el

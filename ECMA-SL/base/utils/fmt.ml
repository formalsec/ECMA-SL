include Format

type t = Format.formatter

let pp_int (fmt : t) (i : int) : unit = pp_print_int fmt i
let pp_float (fmt : t) (f : float) : unit = pp_print_float fmt f
let pp_char (fmt : t) (c : char) : unit = pp_print_char fmt c
let pp_str (fmt : t) (s : string) : unit = pp_print_string fmt s
let pp_bool (fmt : t) (b : bool) : unit = pp_print_bool fmt b

let pp_iter (pp_sep : t -> unit -> unit) (iter : ('a -> unit) -> 'b -> unit)
  (pp_el : t -> 'a -> unit) (fmt : t) (el : 'b) : unit =
  let is_first = ref true in
  let pp_el' el =
    if !is_first then is_first := false else pp_sep fmt ();
    pp_el fmt el
  in
  iter pp_el' el

let pp_hashtbl (sep : string) (pp_el : t -> 'a * 'b -> unit) (fmt : t)
  (htbl : ('a, 'b) Hashtbl.t) =
  let hashtbl_iter f tbl = Hashtbl.iter (fun a b -> f (a, b)) tbl in
  pp_iter (fun fmt () -> pp_str fmt sep) hashtbl_iter pp_el fmt htbl

let pp_hashtbl2 ~(pp_sep : t -> unit -> unit) (pp_el : t -> 'a * 'b -> unit)
  (fmt : t) (v : ('a, 'b) Hashtbl.t) =
  let iter f tbl =
    let tbl =
      Hashtbl.to_seq tbl
      |> List.of_seq
      |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
    in
    List.iter f tbl
  in
  pp_iter pp_sep iter pp_el fmt v

let pp_arr (sep : string) (pp_el : t -> 'a -> unit) (fmt : t) (arr : 'a array) =
  pp_iter (fun fmt () -> pp_str fmt sep) Array.iter pp_el fmt arr

let pp_lst (sep : string) (pp_el : t -> 'a -> unit) (fmt : t) (lst : 'a list) =
  pp_print_list ~pp_sep:(fun fmt () -> pp_str fmt sep) pp_el fmt lst

let pp_seq (sep : string) (pp_el : t -> 'a -> unit) (fmt : t) (seq : 'a Seq.t) =
  pp_print_seq ~pp_sep:(fun fmt () -> pp_str fmt sep) pp_el fmt seq

let pp_opt (pp_el : t -> 'a -> unit) (fmt : t) (el : 'a option) =
  pp_print_option pp_el fmt el

let pp_cond (cond : bool) (pp_el : t -> 'a -> unit) (fmt : t) (el : 'a) =
  if cond then pp_el fmt el

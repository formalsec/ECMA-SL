include Format

type t = Format.formatter

let format : t -> ('a, t, unit) format -> 'a = fprintf
let str : ('a, t, unit, string) format4 -> 'a = asprintf
let ( !> ) : ('a, t, unit, t -> unit) format4 -> 'a = dprintf
let pp_space (ppf : t) () : unit = pp_print_space ppf () [@@inline]
let pp_newline (ppf : t) () : unit = pp_force_newline ppf () [@@inline]
let pp_flush (ppf : t) () : unit = pp_print_flush ppf () [@@inline]
let pp_int (ppf : t) (i : int) : unit = pp_print_int ppf i [@@inline]
let pp_float (ppf : t) (f : float) : unit = pp_print_float ppf f [@@inline]
let pp_char (ppf : t) (c : char) : unit = pp_print_char ppf c [@@inline]
let pp_str (ppf : t) (s : string) : unit = pp_print_string ppf s [@@inline]
let pp_bool (ppf : t) (b : bool) : unit = pp_print_bool ppf b [@@inline]

let pp_opt (pp_v : t -> 'a -> unit) (ppf : t) (v : 'a option) : unit =
  pp_print_option pp_v ppf v
[@@inline]

let pp_lst (pp_sep : t -> unit) (pp_v : t -> 'a -> unit) (ppf : t)
  (lst : 'a list) : unit =
  pp_print_list ~pp_sep:(fun ppf () -> pp_sep ppf) pp_v ppf lst
[@@inline]

let pp_seq (pp_sep : t -> unit) (pp_v : t -> 'a -> unit) (ppf : t)
  (seq : 'a Seq.t) : unit =
  pp_print_seq ~pp_sep:(fun ppf () -> pp_sep ppf) pp_v ppf seq
[@@inline]

let pp_iter (pp_sep : t -> unit) (iter_f : ('a -> unit) -> 'b -> unit)
  (pp_v : t -> 'a -> unit) (ppf : t) (v : 'b) : unit =
  let is_first = ref true in
  let pp_v' v =
    if !is_first then is_first := false else pp_sep ppf;
    pp_v ppf v
  in
  iter_f pp_v' v

let pp_arr (pp_sep : t -> unit) (pp_v : t -> 'a -> unit) (ppf : t)
  (arr : 'a array) =
  pp_iter pp_sep Array.iter pp_v ppf arr

let pp_hashtbl (pp_sep : t -> unit) (pp_v : t -> 'a * 'b -> unit) (ppf : t)
  (htbl : ('a, 'b) Hashtbl.t) =
  let iter_f f = Hashtbl.iter (fun a b -> f (a, b)) in
  pp_iter pp_sep iter_f pp_v ppf htbl

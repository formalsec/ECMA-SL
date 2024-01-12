include Format

let pp_str fmt v = pp_print_string fmt v

let pp_iter sep iter pp_v fmt v =
  let is_first = ref true in
  let pp_v v =
    if !is_first then is_first := false else pp_str fmt sep;
    pp_v fmt v
  in
  iter pp_v v

let pp_arr sep pp_v fmt v = pp_iter sep Array.iter pp_v fmt v

let pp_hashtbl sep pp_v fmt v = pp_iter sep Hashtbl.iter pp_v fmt v

let pp_lst sep pp_v fmt v =
  pp_print_list ~pp_sep:(fun fmt () -> pp_str fmt sep) pp_v fmt v

let pp_seq sep pp_v fmt v =
  pp_print_seq ~pp_sep:(fun fmt () -> pp_str fmt sep) pp_v fmt v

let pp_opt pp_v fmt v = pp_print_option pp_v fmt v

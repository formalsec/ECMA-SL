type bopt =
  | SCLogAnd
  | SCLogOr

let pp_binopt_single fmt = function
  | SCLogAnd -> Format.pp_print_string fmt "&&&"
  | SCLogOr -> Format.pp_print_string fmt "|||"

let str_of_binopt_single (op : bopt) : string =
  Format.asprintf "%a" pp_binopt_single op

let pp_binopt ~pp_v fmt (op, e1, e2) =
  match op with
  | SCLogAnd -> Format.fprintf fmt "%a &&& %a" pp_v e1 pp_v e2
  | SCLogOr -> Format.fprintf fmt "%a ||| %a" pp_v e1 pp_v e2

let str_of_binopt ~pp_v (op : bopt) s1 s2 =
  Format.asprintf "%a" (pp_binopt ~pp_v) (op, s1, s2)

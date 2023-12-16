type bopt =
  | SCLogicalAnd
  | SCLogicalOr

let str_of_binopt (op : bopt) (s1 : string) (s2 : string) : string =
  match op with
  | SCLogicalAnd -> s1 ^ " &&& " ^ s2
  | SCLogicalOr -> s1 ^ " ||| " ^ s2

let str_of_binopt_single (op : bopt) : string =
  match op with
  | SCLogicalAnd -> "&&&"
  | SCLogicalOr -> "|||"

type binopt =
  (* Logical Operators *)
  | SCLogicalAnd
  | SCLogicalOr

let label_of_binopt (op : binopt) : string =
  match op with
  | SCLogicalAnd -> "Logical.sc_and (&&&)"
  | SCLogicalOr -> "Logical.sc_or (|||)"

let str_of_binopt_single (op : binopt) : string =
  match op with SCLogicalAnd -> "&&&" | SCLogicalOr -> "|||"

let str_of_binopt (op : binopt) (s1 : string) (s2 : string) : string =
  match op with
  | SCLogicalAnd -> Printf.sprintf "%s &&& %s" s1 s2
  | SCLogicalOr -> Printf.sprintf "%s ||| %s" s1 s2

type t =
  | PatVar  of string
  | PatVal  of Val.t
  | PatNone


let str (pat_v : t) : string =
  match pat_v with
  | PatVar v -> v
  | PatVal v -> Val.str v
  | PatNone  -> "None"

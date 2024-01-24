open Source

type pv =
  | PatVar of Id.t'
  | PatVal of Val.t
  | PatNone

let pv_pp (fmt : Fmt.t) (pv : pv) : unit =
  match pv with
  | PatVar x -> Fmt.pp_str fmt x
  | PatVal v -> Val.pp fmt v
  | PatNone -> Fmt.pp_str fmt "None"

let pv_str (pv : pv) : string = Fmt.asprintf "%a" pv_pp pv

type t = t' Source.phrase

and t' =
  | ObjPat of (Id.t * pv) list * EPat_metadata.t option
  | DefaultPat

let pp (fmt : Fmt.t) (pat : t) : unit =
  let open Fmt in
  let pp_pb fmt (pbn, pbv) = fprintf fmt "%a: %a" Id.pp pbn pv_pp pbv in
  match pat.it with
  | ObjPat (pbs, _) -> fprintf fmt "{ %a }" (pp_lst ", " pp_pb) pbs
  | DefaultPat -> pp_str fmt "default"

let str (pat : t) : string = Fmt.asprintf "%a" pp pat

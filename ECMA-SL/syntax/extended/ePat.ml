open EslBase
open Source
module Meta = EPat_metadata

type pv = pv' Source.phrase

and pv' =
  | PatVar of Id.t'
  | PatVal of Val.t
  | PatNone

let pv_pp (fmt : Fmt.t) (pv : pv) : unit =
  match pv.it with
  | PatVar x -> Fmt.pp_str fmt x
  | PatVal v -> Val.pp fmt v
  | PatNone -> Fmt.pp_str fmt "None"

let pv_str (pv : pv) : string = Fmt.asprintf "%a" pv_pp pv

type t = t' Source.phrase

and t' =
  | ObjPat of (Id.t * pv) list * Meta.t option
  | DefaultPat

let pp (fmt : Fmt.t) (pat : t) : unit =
  let open Fmt in
  let pp_pb fmt (pbn, pbv) = fprintf fmt "%a: %a" Id.pp pbn pv_pp pbv in
  match pat.it with
  | ObjPat (pbs, _) -> fprintf fmt "{ %a }" (pp_lst ", " pp_pb) pbs
  | DefaultPat -> pp_str fmt "default"

let str (pat : t) : string = Fmt.asprintf "%a" pp pat

let patval_opt (pat : t) (id : Id.t) : pv option =
  let find_pbn (pbn, _) = id.it = pbn.it in
  let get_pbv (_, pbv) = pbv in
  match pat.it with
  | ObjPat (pbs, _) -> Option.map get_pbv (List.find_opt find_pbn pbs)
  | DefaultPat -> None

let patval_remove (pat : t) (id : Id.t) : t =
  let rec patval_remove' = function
    | [] -> []
    | (pbn, _) :: pbs' when id.it = pbn.it -> pbs'
    | pb :: pbs' -> pb :: patval_remove' pbs'
  in
  match pat.it with
  | ObjPat (pbs, meta) -> ObjPat (patval_remove' pbs, meta) @> pat.at
  | DefaultPat -> pat

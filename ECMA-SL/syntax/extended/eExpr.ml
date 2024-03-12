open EslBase
open Source

type t = t' Source.phrase

and t' =
  | Val of Val.t
  | Var of Id.t'
  | GVar of Id.t'
  | Const of Operator.const
  | UnOpt of Operator.unopt * t
  | BinOpt of Operator.binopt * t * t
  | TriOpt of Operator.triopt * t * t * t
  | NOpt of Operator.nopt * t list
  | Call of t * t list * Id.t option
  | ECall of Id.t * t list
  | NewObj of (Id.t * t) List.t
  | Lookup of t * t
  | Curry of t * t list
  | Symbolic of Type.t * t

let rec pp (fmt : Fmt.t) (e : t) : unit =
  let open Fmt in
  match e.it with
  | Val v -> Val.pp fmt v
  | Var x -> pp_str fmt x
  | GVar x -> fprintf fmt "|%s|" x
  | Const c -> Operator.pp_of_const fmt c
  | UnOpt (op, e') -> Operator.pp_of_unopt pp fmt (op, e')
  | BinOpt (op, e1, e2) -> Operator.pp_of_binopt pp fmt (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.pp_of_triopt pp fmt (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.pp_of_nopt pp fmt (op, es)
  | Call (fe, es, ferr) ->
    let pp_catch fmt ferr = fprintf fmt " catch %a" Id.pp ferr in
    fprintf fmt "%a(%a)%a" pp fe (pp_lst ", " pp) es (pp_opt pp_catch) ferr
  | ECall (fn, es) -> fprintf fmt "extern %a(%a)" Id.pp fn (pp_lst ", " pp) es
  | NewObj flds ->
    let pp_fld fmt (fn, fe) = fprintf fmt "%a: %a" Id.pp fn pp fe in
    if List.length flds = 0 then pp_str fmt "{}"
    else fprintf fmt "{ %a }" (pp_lst ", " pp_fld) flds
  | Lookup (oe, fe) -> fprintf fmt "%a[%a]" pp oe pp fe
  | Curry (fe, es) -> fprintf fmt "{%a}@(%a)" pp fe (pp_lst ", " pp) es
  | Symbolic (t, e') -> fprintf fmt "se_mk_symbolic(%a, %a)" Type.pp t pp e'

let str (e : t) : string = Fmt.asprintf "%a" pp e
let isvoid (e : t) : bool = match e.it with Val Val.Void -> true | _ -> false

let rec map (mapper : t -> t) (e : t) : t =
  let map' = map mapper in
  let mapper' e' = mapper (e' @> e.at) in
  mapper'
  @@
  match e.it with
  | (Val _ | Var _ | GVar _ | Const _ | Symbolic _) as e' -> e'
  | UnOpt (op, e') -> UnOpt (op, map' e')
  | BinOpt (op, e1, e2) -> BinOpt (op, map' e1, map' e2)
  | TriOpt (op, e1, e2, e3) -> TriOpt (op, map' e1, map' e2, map' e3)
  | NOpt (op, es) -> NOpt (op, List.map map' es)
  | Call (fe, es, ferr) -> Call (map' fe, List.map map' es, ferr)
  | ECall (fn, es) -> ECall (fn, List.map map' es)
  | NewObj flds -> NewObj (List.map (fun (fn, fe) -> (fn, map' fe)) flds)
  | Lookup (oe, fe) -> Lookup (map' oe, map' fe)
  | Curry (fe, es) -> Curry (map' fe, List.map map' es)

module Mapper = struct
  let id (e : t) : t = e

  let var (subst : (string, t) Hashtbl.t) (e : t) : t =
    let find_subst x = Hashtbl.find_opt subst x in
    let subst_f e =
      match e.it with
      | Var x -> (Option.value ~default:e) (find_subst x)
      | _ -> e
    in
    map subst_f e
end

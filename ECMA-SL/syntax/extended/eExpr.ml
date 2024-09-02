open EslBase
open Source

type t = t' Source.t

and t' =
  | Val of Value.t
  | Var of Id.t'
  | GVar of Id.t'
  | UnOpt of Operator.unopt * t
  | BinOpt of Operator.binopt * t * t
  | TriOpt of Operator.triopt * t * t * t
  | NOpt of Operator.nopt * t list
  | Call of t * t list * Id.t option
  | ECall of Id.t * t list
  | NewObj of (Id.t * t) List.t
  | Lookup of t * t
  | Curry of t * t list

let default : unit -> t =
  let dlft = Val Value.null @> none in
  fun () -> dlft

let isvoid (e : t) : bool = match e.it with Val Unit -> true | _ -> false

let pp_lookup (ppf : Fmt.t) (fe : t) : unit =
  match fe.it with
  | Val (Str fn) -> Fmt.fmt ppf ".%s" fn
  | _ -> Fmt.fmt ppf "[%a]" pp fe

let rec pp (ppf : Fmt.t) (e : t) : unit =
  let pp_vs pp_v ppf vs = Fmt.(pp_lst !>", " pp_v) ppf vs in
  let pp_catch' ppf feh = Fmt.fmt ppf " catch %a" Id.pp feh in
  let pp_catch ppf feh = Fmt.pp_opt pp_catch' ppf feh in
  match e.it with
  | Val v -> Value.pp ppf v
  | Var x -> Fmt.pp_str ppf x
  | GVar x -> Fmt.fmt ppf "|%s|" x
  | UnOpt (op, e') -> Operator.unopt_pp ~pp_v:pp ppf (op, e')
  | BinOpt (op, e1, e2) -> Operator.binopt_pp ~pp_v:pp ppf (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.triopt_pp ~pp_v:pp ppf (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.nopt_pp ~pp_v:pp ppf (op, es)
  | Call (fe, es, feh) -> (
    match fe.it with
    | Val (Str fn) -> Fmt.fmt ppf "%s(%a)%a" fn (pp_vs pp) es pp_catch feh
    | _ -> Fmt.fmt ppf "{%a}(%a)%a" pp fe (pp_vs pp) es pp_catch feh )
  | ECall (fn, es) -> Fmt.fmt ppf "extern %a(%a)" Id.pp fn (pp_vs pp) es
  | NewObj flds ->
    let pp_fld ppf (fn, fe) = Fmt.fmt ppf "%a: %a" Id.pp fn pp fe in
    if List.length flds == 0 then Fmt.pp_str ppf "{}"
    else Fmt.fmt ppf "{ %a }" (pp_vs pp_fld) flds
  | Lookup (oe, fe) -> Fmt.fmt ppf "%a%a" pp oe pp_lookup fe
  | Curry (fe, es) -> Fmt.fmt ppf "{%a}@(%a)" pp fe (pp_vs pp) es

let str (e : t) : string = Fmt.str "%a" pp e [@@inline]

let rec map (mapper : t -> t) (e : t) : t =
  let map' e' = map mapper e' in
  let mapper' e' = mapper (e' @> e.at) in
  mapper'
  @@
  match e.it with
  | (Val _ | Var _ | GVar _) as e' -> e'
  | UnOpt (op, e') -> UnOpt (op, map' e')
  | BinOpt (op, e1, e2) -> BinOpt (op, map' e1, map' e2)
  | TriOpt (op, e1, e2, e3) -> TriOpt (op, map' e1, map' e2, map' e3)
  | NOpt (op, es) -> NOpt (op, List.map map' es)
  | Call (fe, es, feh) -> Call (map' fe, List.map map' es, feh)
  | ECall (fn, es) -> ECall (fn, List.map map' es)
  | NewObj flds -> NewObj (List.map (fun (fn, fe) -> (fn, map' fe)) flds)
  | Lookup (oe, fe) -> Lookup (map' oe, map' fe)
  | Curry (fe, es) -> Curry (map' fe, List.map map' es)

module Mapper = struct
  let id : t -> t = Fun.id

  let var (subst : (string, t) Hashtbl.t) : t -> t =
    map @@ fun e ->
    match e.it with
    | Var x -> Hashtbl.find_opt subst x |> Option.value ~default:e
    | _ -> e
end

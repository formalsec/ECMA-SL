type t =
  | Val of Val.t
  | Var of string
  | GVar of string
  | Const of Operator.const
  | UnOpt of Operator.unopt * t
  | BinOpt of Operator.binopt * t * t
  | TriOpt of Operator.triopt * t * t * t
  | NOpt of Operator.nopt * t list
  | Call of t * t list * string option
  | ECall of string * t list
  | NewObj of (string * t) List.t
  | Lookup of t * t
  | Curry of t * t list
  | Symbolic of Type.t * t

module Parser = struct
  let parse_object_fields (flds : (string * t) list) : (string * t) list =
    let check_duplicates checked (fn, _) =
      if not (Hashtbl.mem checked fn) then Hashtbl.replace checked fn ()
      else failwith "TEMP: Replace by Eslerr.Compile.DuplicateField"
    in
    List.iter (check_duplicates (Hashtbl.create (List.length flds))) flds;
    flds
end

let rec pp (fmt : Fmt.t) (e : t) : unit =
  let open Fmt in
  match e with
  | Val v -> Val.pp fmt v
  | Var x -> pp_str fmt x
  | GVar x -> fprintf fmt "|%s|" x
  | Const c -> Operator.pp_of_const fmt c
  | UnOpt (op, e') -> Operator.pp_of_unopt pp fmt (op, e')
  | BinOpt (op, e1, e2) -> Operator.pp_of_binopt pp fmt (op, e1, e2)
  | TriOpt (op, e1, e2, e3) -> Operator.pp_of_triopt pp fmt (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.pp_of_nopt pp fmt (op, es)
  | Call (fe, es, ferr) ->
    let pp_catch fmt ferr = fprintf fmt " catch %s" ferr in
    fprintf fmt "%a(%a)%a" pp fe (pp_lst ", " pp) es (pp_opt pp_catch) ferr
  | ECall (fn, es) -> fprintf fmt "extern %s(%a)" fn (pp_lst ", " pp) es
  | NewObj flds ->
    let pp_fld fmt (fn, fe) = fprintf fmt "%s: %a" fn pp fe in
    if List.is_empty flds then pp_str fmt "{}"
    else fprintf fmt "{ %a }" (pp_lst ", " pp_fld) flds
  | Lookup (oe, fe) -> fprintf fmt "%a[%a]" pp oe pp fe
  | Curry (fe, es) -> fprintf fmt "{%a}@(%a)" pp fe (pp_lst ", " pp) es
  | Symbolic (t, e') -> fprintf fmt "se_mk_symbolic(%a, %a)" Type.pp t pp e'

let str (e : t) : string = Fmt.asprintf "%a" pp e

let rec map (mapper : t -> t) (e : t) : t =
  let map' = map mapper in
  mapper
  @@
  match e with
  | Val _ | Var _ | GVar _ | Const _ | Symbolic _ -> e
  | UnOpt (op, e') -> UnOpt (op, map' e')
  | BinOpt (op, e1, e2) -> BinOpt (op, map' e1, map' e2)
  | TriOpt (op, e1, e2, e3) -> TriOpt (op, map' e1, map' e2, map' e3)
  | NOpt (op, es) -> NOpt (op, List.map map' es)
  | Call (fe, es, ferr) -> Call (map' fe, List.map map' es, ferr)
  | ECall (fn, es) -> ECall (fn, List.map map' es)
  | NewObj flds -> NewObj (List.map (fun (fn, fe) -> (fn, map' fe)) flds)
  | Lookup (oe, fe) -> Lookup (map' oe, map' fe)
  | Curry (fe, es) -> Curry (map' fe, List.map map' es)

(* FIXME: Requires cleaning below *)
type subst_t = (string, t) Hashtbl.t

let make_subst (xs_es : (string * t) list) : subst_t =
  let subst = Hashtbl.create !Config.default_hashtbl_sz in
  List.iter (fun (x, e) -> Hashtbl.replace subst x e) xs_es;
  subst

let get_subst_o (sbst : subst_t) (x : string) : t option =
  Hashtbl.find_opt sbst x

let get_subst (sbst : subst_t) (x : string) : t =
  let eo = get_subst_o sbst x in
  Option.value ~default:(Var x) eo

let subst (sbst : subst_t) (e : t) : t =
  (* Printf.printf "In subst expr\n"; *)
  let f e' = match e' with Var x -> get_subst sbst x | _ -> e' in
  map f e

let string_of_subst (sbst : subst_t) : string =
  let strs = Hashtbl.fold (fun x e ac -> (x ^ ": " ^ str e) :: ac) sbst [] in
  String.concat ", " strs

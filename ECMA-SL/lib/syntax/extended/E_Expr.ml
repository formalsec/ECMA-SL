type t =
  | Val of Val.t
  | Var of string
  | GVar of string
  | Const of Oper.const
  | BinOpt of Oper.bopt * t * t
  | TriOpt of Oper.topt * t * t * t
  | EBinOpt of EOper.bopt * t * t  (** non-shared binary operators *)
  | UnOpt of Oper.uopt * t
  | NOpt of Oper.nopt * t list
  | Call of t * t list * string option
  | ECall of string * t list
  | NewObj of (string * t) list
  | Lookup of t * t
  | Curry of t * t list
  | Symbolic of Type.t * string

type subst_t = (string, t) Hashtbl.t

let rec str (e : t) : string =
  let str_es es = String.concat ", " (List.map str es) in
  match e with
  | Val n -> Val.str n
  | Var x -> x
  | GVar x -> "|" ^ x ^ "|"
  | Const c -> Oper.str_of_const c
  | UnOpt (op, e) -> Oper.str_of_unopt op ^ "(" ^ str e ^ ")"
  | EBinOpt (op, e1, e2) -> EOper.str_of_binopt op (str e1) (str e2)
  | BinOpt (op, e1, e2) -> Oper.str_of_binopt op (str e1) (str e2)
  | TriOpt (op, e1, e2, e3) -> Oper.str_of_triopt op (str e1) (str e2) (str e3)
  | NOpt (op, es) -> Oper.str_of_nopt op (List.map str es)
  | ECall (f, es) -> "extern " ^ f ^ "(" ^ str_es es ^ ")"
  | Call (f, es, None) -> str f ^ "(" ^ str_es es ^ ")"
  | Call (f, es, Some g) -> str f ^ "(" ^ str_es es ^ ") catch " ^ g
  | NewObj fes ->
      "{ "
      ^ String.concat ", " (List.map (fun (f, e) -> f ^ ": " ^ str e) fes)
      ^ " }"
  | Lookup (e, f) -> str e ^ "[" ^ str f ^ "]"
  | Curry (f, es) -> str f ^ "@(" ^ str_es es ^ ")"
  | Symbolic (t, x) -> "symbolic(" ^ Type.str t ^ ", \"" ^ x ^ "\")"

(* Used in module HTMLExtensions but not yet terminated.
   This still contains defects. *)
let rec pattern_match (subst : subst_t) (e1 : t) (e2 : t) : bool =
  match (e1, e2) with
  | Val v1, Val v2 -> v1 = v2
  | Var x1, Var x2 | GVar x1, GVar x2 -> (
      let x1' = Hashtbl.find_opt subst x1 in
      match x1' with
      | None ->
          Hashtbl.replace subst x1 e2;
          true
      | Some e2' -> e2 = e2')
  | Const c1, Const c2 -> c1 = c2
  | UnOpt (op, e), UnOpt (op', e') when op = op' -> pattern_match subst e e'
  | BinOpt (op, e1, e2), BinOpt (op', e1', e2') when op = op' ->
      pattern_match subst e1 e1' && pattern_match subst e2 e2'
  | Call (f, es, None), Call (f', es', None)
    when List.length es = List.length es' ->
      let b = pattern_match subst f f' in
      if b then
        List.for_all
          (fun (e1, e2) -> pattern_match subst e1 e2)
          (List.combine es es')
      else false
  | _ -> false

let make_subst (xs_es : (string * t) list) : subst_t =
  let subst = Hashtbl.create Common.default_hashtable_size in
  List.iter (fun (x, e) -> Hashtbl.replace subst x e) xs_es;
  subst

let get_subst_o (sbst : subst_t) (x : string) : t option =
  Hashtbl.find_opt sbst x

let get_subst (sbst : subst_t) (x : string) : t =
  let eo = get_subst_o sbst x in
  Option.default (Var x) eo

let rec map (f : t -> t) (e : t) : t =
  let mapf = map f in
  let map_obj = List.map (fun (x, e) -> (x, mapf e)) in
  let e' =
    match e with
    | Val _ | Var _ | Const _ | GVar _ | Symbolic _ -> e
    | UnOpt (op, e) -> UnOpt (op, mapf e)
    | EBinOpt (op, e1, e2) -> EBinOpt (op, mapf e1, mapf e2)
    | BinOpt (op, e1, e2) -> BinOpt (op, mapf e1, mapf e2)
    | TriOpt (op, e1, e2, e3) -> TriOpt (op, mapf e1, mapf e2, mapf e3)
    | NOpt (op, es) -> NOpt (op, List.map mapf es)
    | Call (ef, es, g) -> Call (mapf ef, List.map mapf es, g)
    | ECall (f, es) -> ECall (f, List.map mapf es)
    | NewObj fes -> NewObj (map_obj fes)
    | Lookup (e, ef) -> Lookup (mapf e, mapf ef)
    | Curry (e, es) -> Curry (mapf e, List.map mapf es)
  in
  f e'

let subst (sbst : subst_t) (e : t) : t =
  (* Printf.printf "In subst expr\n"; *)
  let f e' = match e' with Var x -> get_subst sbst x | _ -> e' in
  map f e

let string_of_subst (sbst : subst_t) : string =
  let strs = Hashtbl.fold (fun x e ac -> (x ^ ": " ^ str e) :: ac) sbst [] in
  String.concat ", " strs

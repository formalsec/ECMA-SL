type t =
  | Val     of Val.t
  | Var     of string
  | GVar    of string
  | Const   of Oper.const
  | BinOpt  of Oper.bopt * t * t
  | EBinOpt of EOper.bopt * t * t    (** non-shared binary operators *)
  | UnOpt   of Oper.uopt * t
  | NOpt    of Oper.nopt * t list
  | Call    of t * t list * string option
  | NewObj  of (string * t) list
  | Lookup  of t * t
  | Curry   of t * t list 

type subst_t = (string, t) Hashtbl.t

let rec str (e : t) : string =
  let str_es es = String.concat ", " (List.map str es) in 
  match e with
  | Val n                 -> Val.str n
  | Var x                 -> x
  | GVar x                -> "|" ^ x ^ "|"
  | Const c               -> Oper.str_of_const c
  | UnOpt (op, e)         -> (Oper.str_of_unopt op) ^ "(" ^ (str e) ^ ")"
  | EBinOpt (op, e1, e2)  -> EOper.str_of_binopt op (str e1) (str e2)
  | BinOpt (op, e1, e2)   -> Oper.str_of_binopt op (str e1) (str e2)
  | NOpt (op, es)         -> Oper.str_of_nopt op (List.map str es)
  | Call (f, es, None)    -> Printf.sprintf "%s(%s)" (str f) (String.concat ", " (List.map str es))
  | Call (f, es, Some g)  -> Printf.sprintf "%s(%s) catch %s" (str f) (String.concat ", " (List.map str es)) g
  | NewObj (fes)          -> "{ " ^ (String.concat ", " (List.map (fun (f, e) -> f ^ ": " ^ (str e)) fes)) ^ " }"
  | Lookup (e, f)         -> str e ^ "[" ^ str f ^ "]"
  | Curry (f, es)         -> Printf.sprintf "%s@(%s)" (str f) (str_es es)

let make_subst (xs_es : (string * t) list) : subst_t =
  let subst = Hashtbl.create 31 in
  List.iter (fun (x, e) ->
    Hashtbl.replace subst x e
  ) xs_es;
  subst

let get_subst_o (sbst : subst_t) (x : string) : t option =
  Hashtbl.find_opt sbst x

let get_subst (sbst : subst_t) (x : string) : t =
  let eo = get_subst_o sbst x in
  Option.default (Var x) eo


let rec map (f : (t -> t)) (e : t) : t =
  let mapf = map f in
  let map_obj = List.map (fun (x, e) -> (x, mapf e)) in
  let e' =
    match e with
      | Val _ | Var _
      | Const _ | GVar _         -> e
      | UnOpt (op, e)            -> UnOpt (op, mapf e)
      | EBinOpt (op, e1, e2)     -> EBinOpt (op, mapf e1, mapf e2)
      | BinOpt (op, e1, e2)      -> BinOpt (op, mapf e1, mapf e2)
      | NOpt (op, es)            -> NOpt (op, List.map mapf es)
      | Call (ef, es, g)         -> Call (mapf ef, List.map mapf es, g)
      | NewObj (fes)             -> NewObj (map_obj fes)
      | Lookup (e, ef)           -> Lookup (mapf e, mapf ef) 
      | Curry (e, es)            -> Curry (mapf e, List.map mapf es) in
  f e'

let subst (sbst : subst_t) (e: t) : t =
  (* Printf.printf "In subst expr\n"; *)
  let f e' =
    match e' with
    | Var x -> get_subst sbst x
    | _ -> e' in
  map f e

let string_of_subst (sbst : subst_t) : string =
  let strs =
    Hashtbl.fold
      (fun x e ac -> (x ^ ": " ^ (str e))::ac)
      sbst
      [] in
  String.concat ", " strs
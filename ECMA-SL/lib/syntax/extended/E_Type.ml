type t =
  | AnyType
  | NumberType
  | StringType
  | BooleanType
  | ObjectType of (string, t) Hashtbl.t * t option
  | UserDefinedType of string
  | LiteralType of Val.t
  | ListType of t
  | TupleType of t list
  | UnionType of t list

module Prop = struct
  type p_t = NamedProp of string * t | SumryProp of t
end

let get_named_prps (o : (string, t) Hashtbl.t * t option) : (string * t) list =
  Hashtbl.fold (fun name t prps -> (name, t) :: prps) (fst o) []

let get_sumry_prp (o : (string, t) Hashtbl.t * t option) : t option = snd o

let get_prp_list (o : (string, t) Hashtbl.t * t option) : (string * t) list =
  List.append (get_named_prps o)
    (match get_sumry_prp o with Some t -> [ ("*", t) ] | None -> [])

let rec str (tp : t) : string =
  match tp with
  | AnyType -> "any"
  | NumberType -> "number"
  | StringType -> "string"
  | BooleanType -> "boolean"
  | ObjectType (named_prps, sumry_prp) ->
      let prps = get_prp_list (named_prps, sumry_prp) in
      "{ "
      ^ String.concat ", " (List.map (fun p -> fst p ^ ": " ^ str (snd p)) prps)
      ^ " }"
  | UserDefinedType tp' -> tp'
  | LiteralType v -> Val.str v
  | ListType tp' -> "[" ^ str tp' ^ "]"
  | TupleType tp ->
      "(" ^ String.concat " * " (List.map (fun el -> str el) tp) ^ ")"
  | UnionType tp ->
      "(" ^ String.concat " | " (List.map (fun el -> str el) tp) ^ ")"

let parse_obj_prps (prps : Prop.p_t list) : (string, t) Hashtbl.t * t option =
  let p_t_split p (named_prps, sumry_prps) =
    match p with
    | Prop.NamedProp (name, t) -> ((name, t) :: named_prps, sumry_prps)
    | Prop.SumryProp t -> (named_prps, t :: sumry_prps)
  in
  let named_prp, sumry_prp = List.fold_right p_t_split prps ([], []) in
  let named_prps = Hashtbl.create !Config.default_hashtbl_sz in
  let _ =
    List.iter
      (fun (p : string * t) ->
        match Hashtbl.find_opt named_prps (fst p) with
        | None -> Hashtbl.replace named_prps (fst p) (snd p)
        | Some _ ->
            invalid_arg ("Property '" ^ fst p ^ "' already exists in the object"))
      named_prp
  in
  match sumry_prp with
  | [] -> (named_prps, None)
  | t :: [] -> (named_prps, Some t)
  | default -> invalid_arg "Duplicate summary property in the object"

let merge_tuple_type (nary_tp : t) (simple_tp : t) : t =
  match nary_tp with
  | TupleType nary_tp' -> TupleType (List.append nary_tp' [ simple_tp ])
  | default -> TupleType [ nary_tp; simple_tp ]

let merge_union_type (nary_tp : t) (simple_tp : t) : t =
  match nary_tp with
  | UnionType nary_tp' -> UnionType (List.append nary_tp' [ simple_tp ])
  | default -> UnionType [ nary_tp; simple_tp ]

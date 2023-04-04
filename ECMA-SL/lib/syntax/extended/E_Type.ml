type t =
  | AnyType
  | UndefinedType
  | UnknownType
  | TypeType
  | NumberType
  | StringType
  | BooleanType
  | SymbolType
  | LiteralType of Val.t
  | ObjectType of obj_t
  | ListType of t
  | TupleType of t list
  | UnionType of t list
  | UserDefinedType of string

and obj_t = { flds : (string, field_t) Hashtbl.t; smry : t option }
and field_t = { t : t; opt : bool }

module Field = struct
  type ft = NamedField of string * (t * bool) | SumryField of t
end

let get_obj_fields (tobj : obj_t) : (string, field_t) Hashtbl.t = tobj.flds
let get_obj_smry (tobj : obj_t) : t option = tobj.smry

let get_obj_field_list (tobj : obj_t) : (string * field_t) list =
  List.append
    (Hashtbl.fold (fun fn ft r -> (fn, ft) :: r) tobj.flds [])
    (match tobj.smry with
    | Some smry' -> [ ("*", { t = smry'; opt = false }) ]
    | None -> [])

let get_field_type (tfld : field_t) : t = tfld.t
let is_field_optional (tfld : field_t) : bool = tfld.opt

let rec str (t : t) : string =
  match t with
  | AnyType -> "any"
  | UndefinedType -> "undefined"
  | UnknownType -> "unknown"
  | TypeType -> "_$type"
  | NumberType -> "number"
  | StringType -> "string"
  | BooleanType -> "boolean"
  | SymbolType -> "symbol"
  | LiteralType v -> Val.str v
  | ObjectType t' ->
      let field_opt_str ft = if ft.opt then "?" else "" in
      let field_str (fn, ft) = fn ^ field_opt_str ft ^ ": " ^ str ft.t in
      let fields = get_obj_field_list t' in
      "{ " ^ String.concat ", " (List.map (fun f -> field_str f) fields) ^ " }"
  | ListType t' -> "[" ^ str t' ^ "]"
  | TupleType t ->
      "(" ^ String.concat " * " (List.map (fun el -> str el) t) ^ ")"
  | UnionType t ->
      "(" ^ String.concat " | " (List.map (fun el -> str el) t) ^ ")"
  | UserDefinedType t' -> t'

let simplify_type (t : t) : t =
  let unique t tlst = if List.mem t tlst then tlst else t :: tlst in
  let simplify_type' tlst =
    if List.mem AnyType tlst then [ AnyType ]
    else List.fold_right unique tlst []
  in
  match t with
  | UnionType t' -> (
      let t' = simplify_type' t' in
      match t' with [] -> UnknownType | e :: [] -> e | e :: r -> UnionType t')
  | default -> t

let parse_obj_type (field_lst : Field.ft list) : obj_t =
  let field_split_fun f (named_fs, sumry_fs) =
    match f with
    | Field.NamedField (fn, ft) -> ((fn, ft) :: named_fs, sumry_fs)
    | Field.SumryField t -> (named_fs, t :: sumry_fs)
  in
  let field_type_fun (t : t) (opt : bool) : t =
    if opt then simplify_type (UnionType [ t; UndefinedType ]) else t
  in
  let named_fs, sumry_fs = List.fold_right field_split_fun field_lst ([], []) in
  let flds = Hashtbl.create !Config.default_hashtbl_sz in
  let _ =
    List.iter
      (fun (fn, (t, opt)) ->
        match Hashtbl.find_opt flds fn with
        | None -> Hashtbl.replace flds fn { t = field_type_fun t opt; opt }
        | Some _ -> invalid_arg ("Field '" ^ fn ^ "' already in the object."))
      named_fs
  in
  match sumry_fs with
  | [] -> { flds; smry = None }
  | t :: [] -> { flds; smry = Some t }
  | default -> invalid_arg "Duplicated summary field in the object."

let merge_tuple_type (nary_t : t) (simple_t : t) : t =
  match nary_t with
  | TupleType nary_t' -> TupleType (List.append nary_t' [ simple_t ])
  | default -> TupleType [ nary_t; simple_t ]

let merge_union_type (nary_t : t) (simple_t : t) : t =
  match nary_t with
  | UnionType nary_t' -> UnionType (List.append nary_t' [ simple_t ])
  | default -> UnionType [ nary_t; simple_t ]

open E_Macro

module ElementType = struct
  type t =
    | TypeDeclaration of (string * E_Type.t)
    | Procedure of E_Func.t
    | Macro of E_Macro.t
end

type t = {
  mutable file_name : string;
  imports : string list;
  typedefs : (string, E_Type.t) Hashtbl.t;
  funcs : (string, E_Func.t) Hashtbl.t;
  macros : (string, E_Macro.t) Hashtbl.t;
}

let add_type_decls (prog : t) (type_decls : (string * E_Type.t) list) =
  List.iter
    (fun (d : string * E_Type.t) ->
      let id = fst d in
      let t = snd d in
      match Hashtbl.find_opt prog.typedefs id with
      | None -> Hashtbl.replace prog.typedefs id t
      | Some _ ->
          invalid_arg ("Type \"" ^ id ^ "\" already exists in the program"))
    type_decls

let add_funcs (prog : t) (funcs : E_Func.t list) : unit =
  List.iter
    (fun (f : E_Func.t) ->
      let fname = E_Func.get_name f in
      match Hashtbl.find_opt prog.funcs fname with
      | None -> Hashtbl.replace prog.funcs fname f
      | Some _ ->
          invalid_arg
            ("Function \"" ^ fname ^ "\" already exists in the program"))
    funcs

let add_macros (prog : t) (macros : E_Macro.t list) : unit =
  List.iter
    (fun (m : E_Macro.t) ->
      match Hashtbl.find_opt prog.macros m.name with
      | None -> Hashtbl.replace prog.macros m.name m
      | Some _ ->
          invalid_arg ("Macro \"" ^ m.name ^ "\" already exists in the program"))
    macros

let create (imports : string list) (type_decls : (string * E_Type.t) list)
    (funcs : E_Func.t list) (macros : E_Macro.t list) : t =
  let prog =
    {
      file_name = "temporary name";
      imports;
      typedefs = Hashtbl.create !Config.default_hashtbl_sz;
      funcs = Hashtbl.create !Config.default_hashtbl_sz;
      macros = Hashtbl.create !Config.default_hashtbl_sz;
    }
  in
  add_type_decls prog type_decls;
  add_funcs prog funcs;
  add_macros prog macros;
  prog

let get_file_name (prog : t) : string = prog.file_name
let get_typedefs (prog : t) : (string, E_Type.t) Hashtbl.t = prog.typedefs

let get_typedefs_list (prog : t) : (string * E_Type.t) list =
  Hashtbl.fold (fun n t tps -> (n, t) :: tps) prog.typedefs []

let get_func (prog : t) (func : string) : E_Func.t =
  Hashtbl.find prog.funcs func

let get_func_opt (prog : t) (func : string) : E_Func.t option =
  Hashtbl.find_opt prog.funcs func

let get_funcs (prog : t) : E_Func.t list =
  Hashtbl.fold (fun _ f fs -> f :: fs) prog.funcs []

let get_macros (prog : t) : E_Macro.t list =
  Hashtbl.fold (fun _ m ms -> m :: ms) prog.macros []

let get_imports (prog : t) : string list = prog.imports

let set_file_name (prog : t) (file_name : string) : unit =
  prog.file_name <- file_name

let str (prog : t) : string =
  prog.file_name ^ "\n"
  ^ String.concat " " (List.map (fun i -> "import " ^ i) prog.imports)
  ^ Hashtbl.fold
      (fun n t tps ->
        (if tps <> "" then tps ^ "\n" else tps)
        ^ Printf.sprintf "%s := %s" n (E_Type.str t))
      prog.typedefs ""
  ^ Hashtbl.fold
      (fun n v ac ->
        (if ac <> "" then ac ^ "\n" else ac)
        ^ Printf.sprintf "(%s -> %s)" n (E_Func.str v))
      prog.funcs ""

let apply_macros (prog : t) : t =
  let new_funcs =
    Hashtbl.fold
      (fun _ f ac -> E_Func.apply_macros f (Hashtbl.find_opt prog.macros) :: ac)
      prog.funcs []
  in
  let typedefs = get_typedefs_list prog in
  create prog.imports typedefs new_funcs []

let lambdas (p : t) : (string * string list * string list * E_Stmt.t) list =
  Hashtbl.fold (fun _ f ac -> E_Func.lambdas f @ ac) p.funcs []

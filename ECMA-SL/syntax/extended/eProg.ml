open EMacro

module ElementType = struct
  type t =
    | TypeDeclaration of (string * EType.t)
    | Procedure of EFunc.t
    | Macro of EMacro.t
end

type t =
  { mutable file_name : string
  ; imports : string list
  ; typedefs : (string, EType.t) Hashtbl.t
  ; funcs : (string, EFunc.t) Hashtbl.t
  ; macros : (string, EMacro.t) Hashtbl.t
  }

let add_typedefs (prog : t) (typedefs : (string * EType.t) list) =
  List.iter
    (fun (d : string * EType.t) ->
      let (tname, t) = d in
      match Hashtbl.find_opt prog.typedefs tname with
      | None -> Hashtbl.replace prog.typedefs tname t
      | Some _ ->
        invalid_arg ("Type \"" ^ tname ^ "\" already exists in the program") )
    typedefs

let add_funcs (prog : t) (funcs : EFunc.t list) : unit =
  List.iter
    (fun (f : EFunc.t) ->
      let fname = EFunc.get_name f in
      match Hashtbl.find_opt prog.funcs fname with
      | None -> Hashtbl.replace prog.funcs fname f
      | Some _ ->
        invalid_arg ("Function \"" ^ fname ^ "\" already exists in the program")
      )
    funcs

let add_macros (prog : t) (macros : EMacro.t list) : unit =
  List.iter
    (fun (m : EMacro.t) ->
      match Hashtbl.find_opt prog.macros m.name with
      | None -> Hashtbl.replace prog.macros m.name m
      | Some _ ->
        invalid_arg ("Macro \"" ^ m.name ^ "\" already exists in the program")
      )
    macros

let create (imports : string list) (typedefs : (string * EType.t) list)
  (funcs : EFunc.t list) (macros : EMacro.t list) : t =
  let prog =
    { file_name = "temporary name"
    ; imports
    ; typedefs = Hashtbl.create !Config.default_hashtbl_sz
    ; funcs = Hashtbl.create !Config.default_hashtbl_sz
    ; macros = Hashtbl.create !Config.default_hashtbl_sz
    }
  in
  add_typedefs prog typedefs;
  add_funcs prog funcs;
  add_macros prog macros;
  prog

let get_file_name (prog : t) : string = prog.file_name
let get_typedefs (prog : t) : (string, EType.t) Hashtbl.t = prog.typedefs

let get_typedefs_list (prog : t) : (string * EType.t) list =
  Hashtbl.fold (fun n t tps -> (n, t) :: tps) prog.typedefs []

let get_func (prog : t) (func : string) : EFunc.t = Hashtbl.find prog.funcs func

let get_func_opt (prog : t) (func : string) : EFunc.t option =
  Hashtbl.find_opt prog.funcs func

let get_funcs (prog : t) : EFunc.t list =
  Hashtbl.fold (fun _ f fs -> f :: fs) prog.funcs []

let get_macros (prog : t) : EMacro.t list =
  Hashtbl.fold (fun _ m ms -> m :: ms) prog.macros []

let get_imports (prog : t) : string list = prog.imports

let set_file_name (prog : t) (file_name : string) : unit =
  prog.file_name <- file_name

let str (prog : t) : string =
  prog.file_name
  ^ "\n"
  ^ String.concat " " (List.map (fun i -> "import " ^ i) prog.imports)
  ^ Hashtbl.fold
      (fun n t tps ->
        (if tps <> "" then tps ^ "\n" else tps)
        ^ Printf.sprintf "%s := %s" n (EType.str t) )
      prog.typedefs ""
  ^ Hashtbl.fold
      (fun n v ac ->
        (if ac <> "" then ac ^ "\n" else ac)
        ^ Printf.sprintf "(%s -> %s)" n (EFunc.str v) )
      prog.funcs ""

let apply_macros (prog : t) : t =
  let new_funcs =
    Hashtbl.fold
      (fun _ f ac -> EFunc.apply_macros f (Hashtbl.find_opt prog.macros) :: ac)
      prog.funcs []
  in
  let typedefs = get_typedefs_list prog in
  create prog.imports typedefs new_funcs []

let lambdas (p : t) : (string * string list * string list * EStmt.t) list =
  Hashtbl.fold (fun _ f ac -> EFunc.lambdas f @ ac) p.funcs []

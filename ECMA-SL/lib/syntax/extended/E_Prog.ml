open E_Func
open E_Macro

type t = {
  mutable file_name : string;
  imports : string list;
  funcs : (string, E_Func.t) Hashtbl.t;
  macros : (string, E_Macro.t) Hashtbl.t;
}

let add_funcs (prog : t) (funcs : E_Func.t list) : unit =
  List.iter
    (fun (f : E_Func.t) ->
      match Hashtbl.find_opt prog.funcs f.name with
      | None -> Hashtbl.replace prog.funcs f.name f
      | Some _ ->
          invalid_arg
            ("Function \"" ^ f.name ^ "\" already exists in the program"))
    funcs

let add_macros (prog : t) (macros : E_Macro.t list) : unit =
  List.iter
    (fun (m : E_Macro.t) ->
      match Hashtbl.find_opt prog.macros m.name with
      | None -> Hashtbl.replace prog.macros m.name m
      | Some _ ->
          invalid_arg ("Macro \"" ^ m.name ^ "\" already exists in the program"))
    macros

let create (imports : string list) (funcs : E_Func.t list)
    (macros : E_Macro.t list) : t =
  let prog =
    {
      file_name = "temporary name";
      imports;
      funcs = Hashtbl.create Common.default_hashtable_size;
      macros = Hashtbl.create Common.default_hashtable_size;
    }
  in
  add_funcs prog funcs;
  add_macros prog macros;
  prog

let get_file_name (prog : t) : string = prog.file_name

let get_func (prog : t) (func : string) : E_Func.t =
  Hashtbl.find prog.funcs func

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
  create prog.imports new_funcs []

let lambdas (p : t) : (string * string list * string list * E_Stmt.t) list =
  Hashtbl.fold (fun _ f ac -> E_Func.lambdas f @ ac) p.funcs []

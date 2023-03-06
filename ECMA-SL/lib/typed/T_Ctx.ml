type t = {
  prog : E_Prog.t;
  tenv : (string, E_Type.t option) Hashtbl.t;
  tret : E_Type.t option;
}

let create_typing_context (prog : E_Prog.t) : t =
  { prog; tenv = Hashtbl.create !Flags.default_hashtbl_sz; tret = None }

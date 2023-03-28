type t = {
  prog : E_Prog.t;
  mutable func : E_Func.t;
  tenv : (string, E_Type.t) Hashtbl.t;
}

let create (prog : E_Prog.t) : t =
  {
    prog;
    func = E_Prog.get_func prog "main";
    tenv = Hashtbl.create !Config.default_hashtbl_sz;
  }

let copy (tctx : t) : t =
  { prog = tctx.prog; func = tctx.func; tenv = Hashtbl.copy tctx.tenv }

let get_func (tctx : t) : E_Func.t = tctx.func
let set_func (tctx : t) (func : E_Func.t) : unit = tctx.func <- func
let get_tenv (tctx : t) : (string, E_Type.t) Hashtbl.t = tctx.tenv

let get_func_by_name (tctx : t) (fname : string) : E_Func.t option =
  E_Prog.get_func_opt tctx.prog fname

let get_return_t (tctx : t) : E_Type.t option = E_Func.get_return_t tctx.func

let tenv_reset (tctx : t) : (string, E_Type.t) Hashtbl.t =
  let _ = Hashtbl.clear tctx.tenv in
  tctx.tenv

let tenv_find (tctx : t) (x : string) : E_Type.t option =
  Hashtbl.find_opt tctx.tenv x

let tenv_update (tctx : t) (x : string) (t : E_Type.t) : unit =
  Hashtbl.replace tctx.tenv x t

let tenv_intersect (tctx1 : t) (tctx2 : t) : unit =
  let tenv1 = tctx1.tenv in
  let tenv2 = tctx2.tenv in
  Hashtbl.iter
    (fun x t1 ->
      let t2 = Hashtbl.find_opt tenv2 x in
      let t2' = match t2 with None -> E_Type.UnknownType | Some t2' -> t2' in
      Hashtbl.replace tenv1 x (T_Typing.intersect_types t1 t2'))
    tenv1;
  Hashtbl.iter
    (fun x t2 ->
      let t1 = Hashtbl.find_opt tenv1 x in
      let t1' = match t1 with None -> E_Type.UnknownType | Some t1' -> t1' in
      Hashtbl.replace tenv1 x (T_Typing.intersect_types t1' t2))
    tenv2

type t = {
  prog : E_Prog.t;
  mutable func : E_Func.t;
  tref : (string, E_Type.t option) Hashtbl.t;
  tenv : (string, E_Type.t) Hashtbl.t;
}

let create (prog : E_Prog.t) : t =
  {
    prog;
    func = E_Prog.get_func prog "main";
    tref = Hashtbl.create !Config.default_hashtbl_sz;
    tenv = Hashtbl.create !Config.default_hashtbl_sz;
  }

let copy (tctx : t) : t =
  {
    prog = tctx.prog;
    func = tctx.func;
    tref = Hashtbl.copy tctx.tref;
    tenv = Hashtbl.copy tctx.tenv;
  }

let get_func (tctx : t) : E_Func.t = tctx.func
let set_func (tctx : t) (func : E_Func.t) : unit = tctx.func <- func
let get_tref (tctx : t) : (string, E_Type.t option) Hashtbl.t = tctx.tref
let get_tenv (tctx : t) : (string, E_Type.t) Hashtbl.t = tctx.tenv

let get_func_by_name (tctx : t) (fname : string) : E_Func.t option =
  E_Prog.get_func_opt tctx.prog fname

let get_return_t (tctx : t) : E_Type.t option = E_Func.get_return_t tctx.func

let tref_find (tctx : t) (x : string) : E_Type.t option option =
  Hashtbl.find_opt tctx.tref x

let tenv_find (tctx : t) (x : string) : E_Type.t option =
  Hashtbl.find_opt tctx.tenv x

let tref_update (tctx : t) (x : string) (t : E_Type.t option) : unit =
  Hashtbl.replace tctx.tref x t

let tenv_update (tctx : t) (x : string) (t : E_Type.t) : unit =
  Hashtbl.replace tctx.tenv x t

let trefenv_reset (tctx : t) : t =
  let _ = Hashtbl.clear tctx.tref in
  let _ = Hashtbl.clear tctx.tenv in
  tctx

let trefenv_intersect (tctx1 : t) (tctx2 : t) : t =
  let intersect_ref (tref_final : (string, E_Type.t option) Hashtbl.t)
      (tref1 : (string, E_Type.t option) Hashtbl.t)
      (tref2 : (string, E_Type.t option) Hashtbl.t) : unit =
    Hashtbl.iter
      (fun x t1 ->
        let t2 = Hashtbl.find_opt tref2 x in
        let t2' =
          match t2 with None -> Some E_Type.UnknownType | Some t2' -> t2'
        in
        Hashtbl.replace tref_final x (T_Typing.intersect_opt_types t1 t2'))
      tref1
  in
  let intersect_env (tenv_final : (string, E_Type.t) Hashtbl.t)
      (tenv1 : (string, E_Type.t) Hashtbl.t)
      (tenv2 : (string, E_Type.t) Hashtbl.t) : unit =
    Hashtbl.iter
      (fun x t1 ->
        let t2 = Hashtbl.find_opt tenv2 x in
        let t2' =
          match t2 with None -> E_Type.UnknownType | Some t2' -> t2'
        in
        Hashtbl.replace tenv_final x (T_Typing.intersect_types t1 t2'))
      tenv1
  in
  let _ = intersect_ref tctx1.tref tctx1.tref tctx2.tref in
  let _ = intersect_ref tctx1.tref tctx2.tref tctx1.tref in
  let _ = intersect_env tctx1.tenv tctx1.tenv tctx2.tenv in
  let _ = intersect_env tctx1.tenv tctx2.tenv tctx1.tenv in
  tctx1

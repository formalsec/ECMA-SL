open Source

type tvar_t = { tref : E_Type.t; tnarrow : E_Type.t; mut : bool }

let tvar_tref (tvar : tvar_t) : E_Type.t = tvar.tref
let tvar_tnarrow (tvar : tvar_t) : E_Type.t = tvar.tnarrow
let tvar_mut (tvar : tvar_t) : bool = tvar.mut
let tvar_default (t : E_Type.t) : tvar_t = { tref = t; tnarrow = t; mut = true }

let tvar_create (tref : E_Type.t) (tnarrow : E_Type.t) (mut : bool) : tvar_t =
  let tnarrow' = T_Narrowing.create_nt tref tnarrow in
  { tref; tnarrow = tnarrow'; mut }

type t = {
  prog : E_Prog.t;
  mutable func : E_Func.t;
  mutable stmt : E_Stmt.t;
  tenv : (string, tvar_t) Hashtbl.t;
}

let create (prog : E_Prog.t) : t =
  {
    prog;
    func = E_Prog.get_func prog "main";
    stmt = E_Stmt.Skip @@ no_region;
    tenv = Hashtbl.create !Config.default_hashtbl_sz;
  }

let copy (tctx : t) : t =
  {
    prog = tctx.prog;
    func = tctx.func;
    stmt = tctx.stmt;
    tenv = Hashtbl.copy tctx.tenv;
  }

let get_func (tctx : t) : E_Func.t = tctx.func
let set_func (tctx : t) (func : E_Func.t) : unit = tctx.func <- func
let get_stmt (tctx : t) : E_Stmt.t = tctx.stmt
let set_stmt (tctx : t) (stmt : E_Stmt.t) : unit = tctx.stmt <- stmt
let get_tenv (tctx : t) : (string, tvar_t) Hashtbl.t = tctx.tenv

let get_func_by_name (tctx : t) (fname : string) : E_Func.t option =
  E_Prog.get_func_opt tctx.prog fname

let get_return_t (tctx : t) : E_Type.t option = E_Func.get_return_t tctx.func

let tenv_find (tctx : t) (x : string) : tvar_t option =
  Hashtbl.find_opt tctx.tenv x

let tenv_update (tctx : t) (x : string) (t : tvar_t) : unit =
  Hashtbl.replace tctx.tenv x t

let tenv_reset (tctx : t) : t = Hashtbl.clear tctx.tenv |> fun () -> tctx

let tenv_reset_narrowing (tctx : t) : t =
  let reset_fun x tvar =
    Hashtbl.replace tctx.tenv x { tvar with tnarrow = tvar.tref }
  in
  Hashtbl.iter reset_fun tctx.tenv |> fun () -> tctx

let tenv_lock_types (tctx : t) : t =
  let lock_fun x tvar = Hashtbl.replace tctx.tenv x { tvar with mut = false } in
  Hashtbl.iter lock_fun tctx.tenv |> fun () -> tctx

let tenv_intersect (tctx_src : t) (tctxs : t list) : unit =
  let gather_var_fun (tenv : (string, E_Type.t list) Hashtbl.t) (tctx : t) =
    Hashtbl.iter (fun x _ -> Hashtbl.replace tenv x []) tctx.tenv
  in
  let find_types_fun (x : string) : tvar_t list =
    let unknown_var = tvar_default E_Type.UndefinedType in
    List.map (fun tctx -> Option.default unknown_var (tenv_find tctx x)) tctxs
  in
  let typing_var_fun (tenv : (string, tvar_t) Hashtbl.t) (x : string) =
    let tvars = find_types_fun x in
    let rtvars = List.map (fun tvar -> tvar.tref) tvars in
    let ntvars = List.map (fun tvar -> tvar.tnarrow) tvars in
    let mut = List.for_all (fun tvar -> tvar.mut) tvars in
    let rtvar = T_Narrowing.type_narrowing (E_Type.UnionType rtvars) in
    let ntvar = T_Narrowing.type_narrowing (E_Type.UnionType ntvars) in
    Hashtbl.add tenv x { tref = rtvar; tnarrow = ntvar; mut }
  in
  let tenv = tenv_reset tctx_src |> fun tctx -> tctx.tenv in
  let tenv_lst = Hashtbl.create !Config.default_hashtbl_sz in
  List.iter (fun tctx -> gather_var_fun tenv_lst tctx) tctxs;
  Hashtbl.iter (fun x _ -> typing_var_fun tenv x) tenv_lst

open Source

type tenv_t = (string, tvar_t) Hashtbl.t

and tvar_t =
  { at : E_Type.t
  ; rt : E_Type.t
  ; nt : E_Type.t
  ; mt : bool
  }

let get_tvar_at (tvar : tvar_t) : E_Type.t = tvar.at
let get_tvar_rt (tvar : tvar_t) : E_Type.t = tvar.rt
let get_tvar_nt (tvar : tvar_t) : E_Type.t = tvar.nt
let get_tvar_mt (tvar : tvar_t) : bool = tvar.mt
let get_tvar_t (tvar : tvar_t) : E_Type.t * E_Type.t = (tvar.rt, tvar.nt)
let default_tvar (t : E_Type.t) : tvar_t = { at = t; rt = t; nt = t; mt = true }

let create_tvar (at : E_Type.t) (nt : E_Type.t) (mt : bool) : tvar_t =
  let nt' = T_Narrowing.create_narrow_type at nt in
  { at; rt = at; nt = nt'; mt }

type tstate_t =
  | Normal
  | Abrupt
  | EndBlock

type t =
  { prog : E_Prog.t
  ; mutable func : E_Func.t
  ; mutable stmt : E_Stmt.t
  ; mutable state : tstate_t
  ; tenv : tenv_t
  }

let create (prog : E_Prog.t) : t =
  { prog
  ; func = E_Func.default () @> no_region
  ; stmt = E_Stmt.default () @> no_region
  ; state = Normal
  ; tenv = Hashtbl.create !Config.default_hashtbl_sz
  }

let copy (tctx : t) : t =
  { prog = tctx.prog
  ; func = tctx.func
  ; stmt = tctx.stmt
  ; state = tctx.state
  ; tenv = Hashtbl.copy tctx.tenv
  }

let get_func (tctx : t) : E_Func.t = tctx.func
let set_func (tctx : t) (func : E_Func.t) : unit = tctx.func <- func
let get_stmt (tctx : t) : E_Stmt.t = tctx.stmt
let set_stmt (tctx : t) (stmt : E_Stmt.t) : unit = tctx.stmt <- stmt
let get_tstate (tctx : t) : tstate_t = tctx.state
let get_tenv (tctx : t) : tenv_t = tctx.tenv
let set_tstate (tctx : t) (state : tstate_t) : unit = tctx.state <- state

let get_curr_return_t (tctx : t) : E_Type.t option =
  E_Func.get_return_t tctx.func

let get_func_by_name (tctx : t) (fname : string) : E_Func.t option =
  E_Prog.get_func_opt tctx.prog fname

let get_typedefs (tctx : t) : (string, E_Type.t) Hashtbl.t =
  E_Prog.get_typedefs tctx.prog

let merge_tstates (tstate1 : tstate_t) (tstate2 : tstate_t) : tstate_t =
  match (tstate1, tstate2) with
  | (Normal, _) | (_, Normal) -> Normal
  | (Abrupt, Abrupt) -> Abrupt
  | _ -> failwith "Typed ECMA-SL: T_Ctx.merge_tstates"

let tenv_reset (tctx : t) : t = Hashtbl.clear tctx.tenv |> fun () -> tctx

let tenv_find (tctx : t) (x : string) : tvar_t option =
  Hashtbl.find_opt tctx.tenv x

let tenv_update (tctx : t) (x : string) (tvar : tvar_t) : unit =
  Hashtbl.replace tctx.tenv x tvar

let tenv_constrain (tctx : t) (x : string) (t : E_Type.t) : unit =
  let tvar = tenv_find tctx x in
  match tvar with
  | Some tvar' -> tenv_update tctx x { tvar' with rt = t; nt = t }
  | None -> failwith "Typed ECMA-SL: T_Ctx.tenv_constrain"

let tenv_unnarrow (tctx : t) : t =
  let _reset_tvar tvar = { tvar with nt = tvar.rt } in
  let _reset_f x tvar = Hashtbl.replace tctx.tenv x (_reset_tvar tvar) in
  Hashtbl.iter _reset_f tctx.tenv |> fun () -> tctx

let tenv_lock (tctx : t) : t =
  let _lock_tvar tvar = { tvar with mt = false } in
  let _lock_f x tvar = Hashtbl.replace tctx.tenv x (_lock_tvar tvar) in
  Hashtbl.iter _lock_f tctx.tenv |> fun () -> tctx

let tenv_intersect (tctx_src : t) (tctxs : t list) : unit =
  let _gather_tvar_f (tenv : (string, E_Type.t list) Hashtbl.t) (tctx : t) =
    Hashtbl.iter (fun x _ -> Hashtbl.replace tenv x []) tctx.tenv
  in
  let _find_types_f (x : string) : tvar_t list =
    let tvarUndef = default_tvar E_Type.UndefinedType in
    List.map (fun tctx -> Option.default tvarUndef (tenv_find tctx x)) tctxs
  in
  let _typing_var_f (tenv : tenv_t) (x : string) =
    let tvars = _find_types_f x in
    let atvars = List.map get_tvar_at tvars in
    let ntvars = List.map get_tvar_nt tvars in
    let mt = List.for_all get_tvar_mt tvars in
    let at = T_Narrowing.narrow_type (E_Type.UnionType atvars) in
    let nt = T_Narrowing.narrow_type (E_Type.UnionType ntvars) in
    Hashtbl.add tenv x { at; rt = at; nt; mt }
  in
  let tenv_src = tenv_reset tctx_src |> fun tctx -> tctx.tenv in
  let tenv_lst = Hashtbl.create !Config.default_hashtbl_sz in
  List.iter (_gather_tvar_f tenv_lst) tctxs;
  Hashtbl.iter (fun x _ -> _typing_var_f tenv_src x) tenv_lst

open EslBase
open EslSyntax

let type_func_params (f : EFunc.t) (tctx : TCtx.t) : TCtx.t =
  let open EslSyntax.Source in
  let tparam' = function Some t -> t | None -> EType.AnyType @> no_region in
  let tparam pt = TCtx.tvar_create pt (tparam' pt) in
  let set_param (px, pt) =
    match TCtx.tenv_find tctx px with
    | Some _ -> Internal_error.(throw __FUNCTION__ (Expecting "non-dup param"))
    | None -> TCtx.tenv_set tctx px (tparam pt)
  in
  TCtx.tenv_reset tctx;
  let tpxs = EFunc.tparams f in
  List.iter set_param tpxs;
  tctx

let type_func (_ : Id.t') (f : EFunc.t) (tctx : TCtx.t) : TCtx.t =
  type_func_params f tctx

let type_prog (p : EProg.t) : bool =
  let tctx = TCtx.create p in
  let tctx = Hashtbl.fold type_func (EProg.funcs p) tctx in
  tctx.tsafe

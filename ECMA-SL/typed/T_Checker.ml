let terrs_str (terrs : T_Err.t list) : string =
  String.concat "" (List.map T_Err.format terrs)

let set_typedefs (tctx : T_Ctx.t) : unit =
  let typedefs = T_Ctx.get_tdefs tctx in
  Hashtbl.add_seq !T_Typing.typedefs (Hashtbl.to_seq typedefs)

let type_main_func (tctx : T_Ctx.t) : T_Err.t list =
  match T_Ctx.get_func_by_name tctx "main" with
  | None -> [ T_Err.create T_Err.MissingMainFunc ]
  | Some func ->
    let params = EFunc.params' func in
    if List.length params = 0 then []
    else
      let tkn = T_Err.str_tkn (List.nth params 0) in
      [ T_Err.create T_Err.BadMainArgs ~src:(T_Err.func_tkn func) ~tkn ]

let type_function_params (tctx : T_Ctx.t) (func : EFunc.t) : unit =
  let open Source in
  let _tparam = function None -> EType.AnyType | Some t -> t in
  let _ = T_Ctx.tenv_reset tctx in
  let tparams = EFunc.tparams func in
  List.iter
    (fun (param, tparam) ->
      match T_Ctx.tenv_find tctx param.it with
      | None ->
        let tparam' = _tparam tparam |> fun t -> T_Ctx.create_tvar t t true in
        T_Ctx.tenv_update tctx param.it tparam'
      | Some _ ->
        T_Err.raise (T_Err.DuplicatedParam param.it)
          ~src:(T_Err.func_tkn (T_Ctx.get_func tctx))
          ~tkn:(T_Err.str_tkn param.it) )
    tparams

let test_function_return (tctx : T_Ctx.t) (func : EFunc.t) : unit =
  if T_Ctx.get_tstate tctx = T_Ctx.Normal then
    T_Err.raise T_Err.OpenCodePath ~src:(T_Err.func_tkn func)
      ~tkn:(T_Err.str_tkn (EFunc.name' func))

let type_function (tctx : T_Ctx.t) (func : EFunc.t) : T_Err.t list =
  let _throwToErrLst targetFunc =
    try targetFunc tctx func |> fun () -> []
    with T_Err.TypeError terr' -> [ terr' ]
  in
  let _ = T_Ctx.set_func tctx func in
  let _ = T_Ctx.set_tstate tctx T_Ctx.Normal in
  let terrsParams = _throwToErrLst type_function_params in
  let terrsCode = T_Stmt.type_stmt tctx (EFunc.body func) in
  let terrsReturn = _throwToErrLst test_function_return in
  List.concat [ terrsParams; terrsCode; terrsReturn ]

let type_program (prog : EProg.t) : T_Err.t list =
  let tctx = T_Ctx.create prog in
  let _ = set_typedefs tctx in
  let terrMain = type_main_func tctx in
  let terrFuncs =
    Hashtbl.fold
      (fun _ f acc -> type_function tctx f :: acc)
      (EProg.funcs prog) []
  in
  List.concat (List.append [ terrMain ] terrFuncs)

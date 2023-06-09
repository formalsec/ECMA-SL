let type_errors_str (terrs : T_Err.t list) : string =
  String.concat "" (List.map T_Err.format terrs)

let type_function_params (tctx : T_Ctx.t) (func : E_Func.t) : unit =
  let _tparam = function None -> E_Type.AnyType | Some t -> t in
  let _ = T_Ctx.tenv_reset tctx in
  let tparams = E_Func.get_params_t func in
  List.iter
    (fun (param, tparam) ->
      match T_Ctx.tenv_find tctx param with
      | None ->
          let tparam' = _tparam tparam |> fun t -> T_Ctx.create_tvar t t true in
          T_Ctx.tenv_update tctx param tparam'
      | Some _ ->
          T_Err.raise (T_Err.DuplicatedParam param)
            ~src:(T_Err.func_tkn (T_Ctx.get_func tctx))
            ~tkn:(T_Err.str_tkn param))
    tparams

let type_function (tctx : T_Ctx.t) (func : E_Func.t) : T_Err.t list =
  let _ = T_Ctx.set_func tctx func in
  let paramErrs =
    try type_function_params tctx func |> fun () -> []
    with T_Err.TypeError terr' -> [ terr' ]
  in
  List.append paramErrs (T_Stmt.type_stmt tctx (E_Func.get_body func))

let type_program (prog : E_Prog.t) : T_Err.t list =
  let tctx = T_Ctx.create prog in
  List.concat (List.map (type_function tctx) (E_Prog.get_funcs prog))

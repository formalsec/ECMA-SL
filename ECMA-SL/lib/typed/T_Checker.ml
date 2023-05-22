let type_errors_str (terrs : T_Err.t list) : string =
  String.concat "" (List.map (fun terr -> T_Err.format terr) terrs)

let type_function_params (tctx : T_Ctx.t) (func : E_Func.t) : unit =
  let _ = T_Ctx.tenv_reset tctx in
  let tparams = E_Func.get_params_t func in
  List.iter
    (fun (pn, pt) ->
      match T_Ctx.tenv_find tctx pn with
      | None ->
          let pt' = match pt with None -> E_Type.AnyType | Some t -> t in
          T_Ctx.tenv_update tctx pn (T_Ctx.tvar_create pt' pt' true)
      | Some _ ->
          T_Err.raise (T_Err.DuplicatedParam pn)
            ~src:(T_Err.Func (T_Ctx.get_func tctx))
            ~tkn:(T_Err.Str pn))
    tparams

let type_function (tctx : T_Ctx.t) (func : E_Func.t) : T_Err.t list =
  let type_function' () = T_Stmt.type_stmt tctx (E_Func.get_body func) in
  let _ = T_Ctx.set_func tctx func in
  let terr =
    try type_function_params tctx func |> fun () -> []
    with T_Err.TypeError terr' -> [ terr' ]
  in
  List.append terr (type_function' ())

let type_program (prog : E_Prog.t) : T_Err.t list =
  let tctx = T_Ctx.create prog in
  List.concat (List.map (fun f -> type_function tctx f) (E_Prog.get_funcs prog))

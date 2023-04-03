let type_errors_str (terrors : T_Err.t list) : string =
  let terrors_str = List.map (fun terr -> T_Err.format terr) terrors in
  String.concat "" terrors_str

let type_function_params (tctx : T_Ctx.t) (func : E_Func.t) : unit =
  let tparams = E_Func.get_params_t func in
  let _ = T_Ctx.trefenv_reset tctx in
  List.iter
    (fun (x, t) ->
      match T_Ctx.tenv_find tctx x with
      | None ->
          let t' = match t with None -> E_Type.AnyType | Some t' -> t' in
          T_Ctx.tref_update tctx x (Some t');
          T_Ctx.tenv_update tctx x t'
      | Some _ ->
          T_Err.raise (T_Err.DuplicatedParam x)
            ~src:(T_Err.Func (T_Ctx.get_func tctx))
            ~cs:(T_Err.Str x))
    tparams

let type_function (tctx : T_Ctx.t) (func : E_Func.t) : T_Err.t list =
  let type_function' () = T_Stmt.type_stmt tctx (E_Func.get_body func) in
  let _ = T_Ctx.set_func tctx func in
  try
    let _ = type_function_params tctx func in
    type_function' ()
  with T_Err.TypeError t -> t :: type_function' ()

let type_program (prog : E_Prog.t) : T_Err.t list =
  let tctx = T_Ctx.create prog in
  List.concat (List.map (fun f -> type_function tctx f) (E_Prog.get_funcs prog))

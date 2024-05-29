open EslBase
open EslSyntax
open EslSyntax.Source

let rec type_expr (tctx : TCtx.t) (e : EExpr.t) : EType.t =
  type_expr' tctx e @?> e.at

and type_expr' (tctx : TCtx.t) (e : EExpr.t) : EType.t' =
  let texprs es = List.map (type_expr tctx) es in
  let tflds flds = List.map (fun (fn, fe) -> (fn, (type_expr tctx) fe)) flds in
  match e.it with
  | Val v -> type_val v
  | Var x -> type_var tctx (x @?> e.at)
  | GVar _ -> AnyType (* TODO: global variables *)
  | Const c -> TOperator.type_const c
  | UnOpt (op, e') -> texprs [ e' ] |> TOperator.type_unopt op
  | BinOpt (op, e1, e2) -> texprs [ e1; e2 ] |> TOperator.type_binopt op
  | TriOpt (op, e1, e2, e3) -> texprs [ e1; e2; e3 ] |> TOperator.type_triopt op
  | NOpt (op, es) -> texprs es |> TOperator.type_nopt op
  | Call (_, _, None) -> AnyType (* TODO: function calls *)
  | Call (_, _, Some _) -> AnyType (* TODO: function calls with throws *)
  | ECall _ -> AnyType (* TODO: external calls *)
  | NewObj flds -> tflds flds |> type_object
  | Lookup _ -> AnyType (* TODO: field lookups *)
  | Curry _ -> AnyType (* TODO: curry expressions *)
  | Symbolic _ -> AnyType (* TODO: symbolic expression *)

and type_val (v : Val.t) : EType.t' =
  match v with
  | Null -> NullType
  | Void -> VoidType
  | Int i -> LiteralType (LitWeak, IntegerLit i)
  | Flt f -> LiteralType (LitWeak, FloatLit f)
  | Str s -> LiteralType (LitWeak, StringLit s)
  | Bool b -> LiteralType (LitWeak, BooleanLit b)
  | Symbol "undefined" -> UndefinedType
  | Symbol s -> LiteralType (LitWeak, SymbolLit s)
  | Loc _ -> Log.fail "loc val"
  | Arr _ -> Log.fail "array val"
  | List _ -> Log.fail "list val"
  | Tuple _ -> Log.fail "tuple val"
  | Byte _ -> Log.fail "byte val"
  | Type _ -> AnyType (* TODO *)
  | Curry _ -> AnyType (* TODO *)

and type_var (tctx : TCtx.t) (x : Id.t) : EType.t' =
  match TCtx.tenv_find tctx x with
  | None -> Typing_error.(throw ~src:(ErrSrc.at x) (UnknownVar x.it))
  | Some t -> t.tref

and type_object (flds : (Id.t * EType.t) list) : EType.t' =
  let set_object_field_f tflds (fn, ft) =
    if not (Hashtbl.mem tflds fn.it) then
      Hashtbl.replace tflds fn.it (fn, ft, EType.FldReq)
    else Log.fail "unexpected dup object fld"
  in
  let tflds = Hashtbl.create !Base.default_hashtbl_sz in
  List.iter (set_object_field_f tflds) flds;
  ObjectType { kind = ObjLit; flds = tflds; smry = None }

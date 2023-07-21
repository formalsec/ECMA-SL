open Type_of_val

let bin_args_typing_arith (arg1_t : Type.t option) (arg2_t : Type.t option) :
    Type.t option =
  match (arg1_t, arg2_t) with
  | Some Type.IntType, Some Type.IntType -> Some Type.IntType
  | Some Type.FltType, Some Type.IntType -> Some Type.FltType
  | Some Type.IntType, Some Type.FltType -> Some Type.FltType
  | Some Type.FltType, Some Type.FltType -> Some Type.FltType
  | _ -> None

let bin_args_typing_eq (arg1_t : Type.t option) (arg2_t : Type.t option) :
    Type.t option =
  match (arg1_t, arg2_t) with
  | Some Type.IntType, Some Type.IntType
  | Some Type.FltType, Some Type.FltType
  | Some Type.ArrayType, Some Type.ArrayType
  | Some Type.StrType, Some Type.StrType
  | Some Type.BoolType, Some Type.BoolType ->
      Some Type.BoolType
  (* missing the symbolic cases *)
  | _ -> None

let bin_args_typing_comp (arg1_t : Type.t option) (arg2_t : Type.t option) :
    Type.t option =
  match (arg1_t, arg2_t) with
  | Some Type.IntType, Some Type.IntType -> Some Type.BoolType
  | Some Type.FltType, Some Type.FltType -> Some Type.BoolType
  | _ -> None

let bin_args_typing_logic (arg1_t : Type.t option) (arg2_t : Type.t option) :
    Type.t option =
  match (arg1_t, arg2_t) with
  | Some Type.BoolType, Some Type.BoolType -> Some Type.BoolType
  | _ -> None

let bitwise_operators_typing_logic (arg1_t : Type.t option)
    (arg2_t : Type.t option) : Type.t option =
  match (arg1_t, arg2_t) with
  | Some Type.FltType, Some Type.FltType -> Some Type.FltType
  | _ -> None

let bin_args_typing_inlist (arg1_t : Type.t option) : Type.t option =
  match arg1_t with Some Type.ListType -> Some Type.BoolType | _ -> None

let bin_args_typing_lnth (arg1 : Expr.t) (arg2 : Expr.t) : Type.t option =
  match (arg1, arg2) with
  | Expr.Val (Val.List l), Expr.Val (Val.Int i) -> type_of_val (List.nth l i)
  | _, _ -> None

let bin_args_typing_pow (arg1_t : Type.t option) (arg2_t : Type.t option) :
    Type.t option =
  match (arg1_t, arg2_t) with
  | Some Type.FltType, Some Type.FltType -> Some Type.FltType
  | _ -> None

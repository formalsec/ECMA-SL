let un_args_typing_neg (arg_t : Type.t option) : Type.t option =
  match arg_t with
  | Some Type.IntType -> Some Type.IntType
  | Some Type.FltType -> Some Type.FltType
  | _ -> None

let un_args_typing_bitwise_not (arg_t : Type.t option) : Type.t option =
  match arg_t with Some Type.FltType -> Some Type.FltType | _ -> None

let un_args_typing_not (arg_t : Type.t option) : Type.t option =
  match arg_t with Some Type.BoolType -> Some Type.BoolType | _ -> None

let un_args_typing_container_length (arg_t : Type.t option)
    (container_t : Type.t) : Type.t option =
  match (arg_t, container_t) with
  | Some Type.ListType, Type.ListType -> Some Type.IntType
  | Some Type.TupleType, Type.TupleType -> Some Type.IntType
  | _ -> None

let un_args_typing_int_casts (arg_t : Type.t option) (ret_t : Type.t) :
    Type.t option =
  match arg_t with Some Type.IntType -> Some ret_t | _ -> None

let un_args_typing_float_casts (arg_t : Type.t option) (ret_t : Type.t) :
    Type.t option =
  match arg_t with Some Type.FltType -> Some ret_t | _ -> None

let un_args_typing_typeof (arg_t : Type.t option) : Type.t option =
  match arg_t with Some _ -> Some Type.TypeType | None -> None

let un_args_typing_sconcat (arg_t : Type.t option) : Type.t option =
  match arg_t with Some Type.ListType -> Some Type.StrType | _ -> None

let un_args_typing_float_math (arg_t : Type.t option) (ret_t : Type.t) :
    Type.t option =
  match arg_t with Some Type.FltType -> Some ret_t | _ -> None

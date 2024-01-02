module Internal = struct
  type t =
    | Default
    | Custom of string
    | Expecting of string
    | UnexpectedEval of string option
    | NotImplemented of string option
end

module Runtime = struct
  type t =
    | Default
    | Custom of string
    | Unexpected of string
    | OpEvalErr of string
    | UnknownVar of string
    | UnknownLoc of string
    | UnknownFunc of string
    | BadNArgs of int * int
    | BadVal of string * Val.t
    | BadExpr of string * Val.t
    | BadFuncId of Val.t
    | BadOpArgs of string * Val.t list
    | MissingReturn of string
end

let internal_message_str (msg : Internal.t) : string =
  let open Internal in
  match msg with
  | Default -> "generic internal error"
  | Custom msg' -> msg'
  | Expecting msg' -> Printf.sprintf "expecting %s" msg'
  | UnexpectedEval msg' -> (
    match msg' with
    | None -> Printf.sprintf "unexpected evaluation"
    | Some msg'' -> Printf.sprintf "unexpected evaluation of '%s'" msg'' )
  | NotImplemented msg' -> (
    match msg' with
    | None -> Printf.sprintf "not implemented"
    | Some msg'' -> Printf.sprintf "'%s' not implemented" msg'' )

let runtime_message_str (msg : Runtime.t) : string =
  let open Runtime in
  match msg with
  | Default -> "Generic runtime error."
  | Custom msg' -> msg'
  | Unexpected msg -> Printf.sprintf "Unexpected %s." msg
  | OpEvalErr op_lbl -> Printf.sprintf "Exception in Operator.%s." op_lbl
  | UnknownVar x -> Printf.sprintf "Cannot find variable '%s'." x
  | UnknownLoc l -> Printf.sprintf "Cannot find location '%s'." l
  | UnknownFunc fn -> Printf.sprintf "Cannot find function '%s'." fn
  | BadNArgs (nparams, nargs) ->
    Printf.sprintf "Expected %d arguments, but got %d." nparams nargs
  | BadVal (exp_str, v) ->
    Printf.sprintf "Expecting %s value, but got '%s'." exp_str (Val.str v)
  | BadExpr (exp_str, v) ->
    Printf.sprintf "Expecting %s expression, but got '%s'." exp_str (Val.str v)
  | BadFuncId v ->
    Printf.sprintf "Expecting a function identifier, but got '%s'." (Val.str v)
  | BadOpArgs (types_str, vals) ->
    let nargs = List.length vals > 1 in
    let s_sfx = if nargs then "s" else "" in
    let _paren s = if nargs then "(" ^ s ^ ")" else s in
    let vals_str = List.map Val.str vals |> String.concat ", " |> _paren in
    Printf.sprintf "Expecting argument%s of type%s '%s', but got '%s'." s_sfx
      s_sfx types_str vals_str
  | MissingReturn fn -> Printf.sprintf "Missing return in function '%s'." fn

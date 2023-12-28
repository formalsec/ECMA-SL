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
    | OperatorError of string
    | UnexpectedValue of string
    | NExpectedArgs of int * int
    | BadValue of string * Val.t
    | BadExpression of string * Val.t
    | BadOperands of string * Val.t list
    | BadFunctionId of Val.t
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
  | OperatorError op_label ->
    Printf.sprintf "Exception in Operator.%s." op_label
  | UnexpectedValue msg -> Printf.sprintf "Unexpected %s." msg
  | NExpectedArgs (nparams, nargs) ->
    Printf.sprintf "Expected %d arguments, but got %d." nparams nargs
  | BadValue (exp_str, v) ->
    Printf.sprintf "Expecting %s value, but got '%s'." exp_str (Val.str v)
  | BadExpression (exp_str, v) ->
    Printf.sprintf "Expecting %s expression, but got '%s'." exp_str (Val.str v)
  | BadOperands (types_str, vals) ->
    let nargs = List.length vals > 1 in
    let s_sfx = if nargs then "s" else "" in
    let _paren s = if nargs then "(" ^ s ^ ")" else s in
    let vals_str = List.map Val.str vals |> String.concat ", " |> _paren in
    Printf.sprintf "Expecting argument%s of type%s '%s', but got '%s'." s_sfx
      s_sfx types_str vals_str
  | BadFunctionId v ->
    Printf.sprintf "Expecting a function identifier, but got '%s'." (Val.str v)

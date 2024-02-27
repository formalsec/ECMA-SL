module Internal = struct
  type t =
    | Default
    | Custom of string
    | Expecting of string
    | UnexpectedEval of string option
    | NotImplemented of string option
end

module Compile = struct
  type t =
    | Default
    | Custom of string
    | UnknownDependency of Id.t
    | CyclicDependency of Id.t
    | DuplicatedTdef of Id.t
    | DuplicatedFunc of Id.t
    | DuplicatedMacro of Id.t
    | DuplicatedField of Id.t
    | DuplicatedSwitchCase of Val.t
    | UnknownMacro of Id.t
    | BadNArgs of int * int
end

module Runtime = struct
  type t =
    | Default
    | Custom of string
    | Unexpected of string
    | UnexpectedExitVal of Val.t
    | Failure of string
    | UncaughtExn of string
    | OpEvalErr of string
    | UnknownVar of Id.t'
    | UnknownFunc of Id.t'
    | BadNArgs of int * int
    | BadVal of string * Val.t
    | BadExpr of string * Val.t
    | BadFuncId of Val.t
    | BadOpArgs of string * Val.t list
    | MissingReturn of Id.t
end

module ErrFmt = Eslerr_fmt

module InternalFmt : ErrFmt.ERR_TYPE_FMT with type msg = Internal.t = struct
  type msg = Internal.t

  let font () : Font.t = Font.Red
  let header () : string = "InternalError"

  let pp (fmt : Fmt.t) (msg : msg) : unit =
    let open Fmt in
    match msg with
    | Default -> fprintf fmt "generic internal error"
    | Custom msg' -> fprintf fmt "%s" msg'
    | Expecting msg' -> fprintf fmt "expecting %s" msg'
    | UnexpectedEval msg' -> (
      match msg' with
      | None -> fprintf fmt "unexpected evaluation"
      | Some msg'' -> fprintf fmt "unexpected evaluation of '%s'" msg'' )
    | NotImplemented msg' -> (
      match msg' with
      | None -> fprintf fmt "not implemented"
      | Some msg'' -> fprintf fmt "'%s' not implemented" msg'' )

  let str (msg : msg) : string = Fmt.asprintf "%a" pp msg
end

module CompileFmt : ErrFmt.ERR_TYPE_FMT with type msg = Compile.t = struct
  type msg = Compile.t

  let font () : Font.t = Font.Red
  let header () : string = "CompileError"

  let pp (fmt : Fmt.t) (msg : msg) : unit =
    let open Fmt in
    match msg with
    | Default -> fprintf fmt "Generic compilation error."
    | Custom msg' -> fprintf fmt "%s" msg'
    | UnknownDependency file ->
      fprintf fmt "Cannot find dependency '%a'." Id.pp file
    | CyclicDependency file ->
      fprintf fmt "Cyclic dependency of file '%a'." Id.pp file
    | DuplicatedTdef tn ->
      fprintf fmt "Duplicated definition for typedef '%a'." Id.pp tn
    | DuplicatedFunc fn ->
      fprintf fmt "Duplicated definition for function '%a'." Id.pp fn
    | DuplicatedMacro mn ->
      fprintf fmt "Duplicated definition for macro '%a'." Id.pp mn
    | DuplicatedField fn ->
      fprintf fmt "Duplicated field name '%a' for object literal." Id.pp fn
    | DuplicatedSwitchCase v ->
      fprintf fmt "Duplicated case value '%a' for switch statement." Val.pp v
    | UnknownMacro mn -> fprintf fmt "Cannot find macro '%a'." Id.pp mn
    | BadNArgs (nparams, nargs) ->
      fprintf fmt "Expected %d arguments, but got %d." nparams nargs

  let str (msg : msg) : string = Fmt.asprintf "%a" pp msg
end

module RuntimeFmt : ErrFmt.ERR_TYPE_FMT with type msg = Runtime.t = struct
  type msg = Runtime.t

  let font () : Font.t = Font.Red
  let header () : string = "RuntimeError"

  let pp (fmt : Fmt.t) (msg : Runtime.t) : unit =
    let open Fmt in
    match msg with
    | Default -> fprintf fmt "Generic runtime error."
    | Custom msg' -> fprintf fmt "%s" msg'
    | Unexpected msg -> fprintf fmt "Unexpected %s." msg
    | UnexpectedExitVal v -> fprintf fmt "Unexpected exit value '%a'." Val.pp v
    | Failure msg -> fprintf fmt "Failure %s." msg
    | UncaughtExn msg -> fprintf fmt "Uncaught exception %s." msg
    | OpEvalErr op_lbl -> fprintf fmt "Exception in Operator.%s." op_lbl
    | UnknownVar x -> fprintf fmt "Cannot find variable '%s'." x
    | UnknownFunc fn -> fprintf fmt "Cannot find function '%s'." fn
    | BadNArgs (nparams, nargs) ->
      fprintf fmt "Expected %d arguments, but got %d." nparams nargs
    | BadVal (texp, v) ->
      fprintf fmt "Expecting %s value, but got '%a'." texp Val.pp v
    | BadExpr (texp, v) ->
      fprintf fmt "Expecting %s expression, but got '%a'." texp Val.pp v
    | BadFuncId v ->
      fprintf fmt "Expecting a function identifier, but got '%a'." Val.pp v
    | BadOpArgs (texp, vals) when List.length vals = 1 ->
      fprintf fmt "Expecting argument of type '%s', but got '%a'." texp
        (pp_lst ", " Val.pp) vals
    | BadOpArgs (texp, vals) ->
      fprintf fmt "Expecting arguments of types '%s', but got '(%a)'." texp
        (pp_lst ", " Val.pp) vals
    | MissingReturn fn ->
      fprintf fmt "Missing return in function '%a'." Id.pp fn

  let str (msg : msg) : string = Fmt.asprintf "%a" pp msg
end

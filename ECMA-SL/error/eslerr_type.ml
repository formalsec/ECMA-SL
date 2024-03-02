type internal =
  | Default
  | Custom of string
  | Expecting of string
  | UnexpectedEval of string option
  | NotImplemented of string option

type compile =
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

type runtime =
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

module type ERR_TYPE = sig
  type t

  val font : unit -> Font.t
  val header : unit -> string
  val equal : t -> t -> bool
  val pp : Fmt.t -> t -> unit
  val str : t -> string
end

module Internal : ERR_TYPE with type t = internal = struct
  type t = internal

  let font () : Font.t = Font.Red
  let header () : string = "InternalError"
  let equal (msg1 : t) (msg2 : t) : bool = msg1 = msg2

  let pp (fmt : Fmt.t) (msg : t) : unit =
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

  let str (msg : t) : string = Fmt.asprintf "%a" pp msg
end

module Compile : ERR_TYPE with type t = compile = struct
  type t = compile

  let font () : Font.t = Font.Red
  let header () : string = "CompileError"

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (UnknownDependency file1, UnknownDependency file2) -> file1.it = file2.it
    | (CyclicDependency file1, CyclicDependency file2) -> file1.it = file2.it
    | (DuplicatedTdef tn1, DuplicatedTdef tn2) -> tn1.it = tn2.it
    | (DuplicatedFunc fn1, DuplicatedFunc fn2) -> fn1.it = fn2.it
    | (DuplicatedMacro mn1, DuplicatedMacro mn2) -> mn1.it = mn2.it
    | (DuplicatedField fn1, DuplicatedField fn2) -> fn1.it = fn2.it
    | (DuplicatedSwitchCase v1, DuplicatedSwitchCase v2) -> Val.equal v1 v2
    | (UnknownMacro mn1, UnknownMacro mn2) -> mn1.it = mn2.it
    | _ -> msg1 = msg2

  let pp (fmt : Fmt.t) (msg : t) : unit =
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

  let str (msg : t) : string = Fmt.asprintf "%a" pp msg
end

module Runtime : ERR_TYPE with type t = runtime = struct
  type t = runtime

  let font () : Font.t = Font.Red
  let header () : string = "RuntimeError"

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (UnexpectedExitVal v1, UnexpectedExitVal v2) -> Val.equal v1 v2
    | (BadVal (texpr1, v1), BadVal (texpr2, v2)) ->
      texpr1 = texpr2 && Val.equal v1 v2
    | (BadExpr (texpr1, v1), BadExpr (texpr2, v2)) ->
      texpr1 = texpr2 && Val.equal v1 v2
    | (BadFuncId v1, BadFuncId v2) -> Val.equal v1 v2
    | (BadOpArgs (texpr1, vs1), BadOpArgs (texpr2, vs2)) ->
      texpr1 = texpr2 && List.equal Val.equal vs1 vs2
    | (MissingReturn fn1, MissingReturn fn2) -> fn1.it = fn2.it
    | _ -> msg1 = msg2

  let pp (fmt : Fmt.t) (msg : t) : unit =
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
    | BadVal (texpr, v) ->
      fprintf fmt "Expecting %s value, but got '%a'." texpr Val.pp v
    | BadExpr (texpr, v) ->
      fprintf fmt "Expecting %s expression, but got '%a'." texpr Val.pp v
    | BadFuncId v ->
      fprintf fmt "Expecting a function identifier, but got '%a'." Val.pp v
    | BadOpArgs (texpr, vs) when List.length vs = 1 ->
      fprintf fmt "Expecting argument of type '%s', but got '%a'." texpr
        (pp_lst ", " Val.pp) vs
    | BadOpArgs (texpr, vs) ->
      fprintf fmt "Expecting arguments of types '%s', but got '(%a)'." texpr
        (pp_lst ", " Val.pp) vs
    | MissingReturn fn ->
      fprintf fmt "Missing return in function '%a'." Id.pp fn

  let str (msg : t) : string = Fmt.asprintf "%a" pp msg
end

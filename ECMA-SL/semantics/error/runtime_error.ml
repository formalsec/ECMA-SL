open EslBase
open EslSyntax
module ErrSrc = Error_source
module RtTrace = Error_trace

type msg =
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

module RuntimeErr : Error_type.ERROR_TYPE with type t = msg = struct
  type t = msg

  let header : string = "RuntimeError"
  let font : Font.t = [ Red ]

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') -> String.equal msg1' msg2'
    | (Unexpected msg1', Unexpected msg2') -> String.equal msg1' msg2'
    | (UnexpectedExitVal v1, UnexpectedExitVal v2) -> Val.equal v1 v2
    | (Failure msg1', Failure msg2') -> String.equal msg1' msg2'
    | (UncaughtExn msg1', UncaughtExn msg2') -> String.equal msg1' msg2'
    | (OpEvalErr oplbl1, OpEvalErr oplbl2) -> String.equal oplbl1 oplbl2
    | (UnknownVar x1, UnknownVar x2) -> String.equal x1 x2
    | (UnknownFunc fn1, UnknownFunc fn2) -> String.equal fn1 fn2
    | (BadNArgs (npxs1, nargs1), BadNArgs (npxs2, nargs2)) ->
      Int.equal npxs1 npxs2 && Int.equal nargs1 nargs2
    | (BadVal (texpr1, v1), BadVal (texpr2, v2)) ->
      String.equal texpr1 texpr2 && Val.equal v1 v2
    | (BadExpr (texpr1, v1), BadExpr (texpr2, v2)) ->
      String.equal texpr1 texpr2 && Val.equal v1 v2
    | (BadFuncId v1, BadFuncId v2) -> Val.equal v1 v2
    | (BadOpArgs (texpr1, vs1), BadOpArgs (texpr2, vs2)) ->
      String.equal texpr1 texpr2 && List.equal Val.equal vs1 vs2
    | (MissingReturn fn1, MissingReturn fn2) -> Id.equal fn1 fn2
    | _ -> false

  let pp (fmt : Fmt.t) (msg : t) : unit =
    let open Fmt in
    match msg with
    | Default -> fprintf fmt "Generic runtime error."
    | Custom msg' -> fprintf fmt "%s" msg'
    | Unexpected msg -> fprintf fmt "Unexpected %s." msg
    | UnexpectedExitVal v -> fprintf fmt "Unexpected exit value '%a'." Val.pp v
    | Failure msg -> fprintf fmt "Failure %s." msg
    | UncaughtExn msg -> fprintf fmt "Uncaught exception %s." msg
    | OpEvalErr oplbl -> fprintf fmt "Exception in Operator.%s." oplbl
    | UnknownVar x -> fprintf fmt "Cannot find variable '%s'." x
    | UnknownFunc fn -> fprintf fmt "Cannot find function '%s'." fn
    | BadNArgs (npxs, nargs) ->
      fprintf fmt "Expected %d arguments, but got %d." npxs nargs
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

type t =
  { msgs : msg list
  ; src : ErrSrc.t
  ; trace : RtTrace.t option
  }

exception Error of t

let create ?(src : ErrSrc.t = ErrSrc.none ()) (msgs : msg list) : exn =
  Error { msgs; src; trace = None }

let throw ?(src : ErrSrc.t = ErrSrc.none ()) (msg : msg) : 'a =
  raise @@ create ~src [ msg ]

let pp (fmt : Fmt.t) (err : t) : unit =
  let open Fmt in
  let module MsgFmt = Error_type.ErrorTypeFmt (RuntimeErr) in
  let module ErrSrcFmt = ErrSrc.ErrSrcFmt (RuntimeErr) in
  let module RtTraceFmt = RtTrace.RtTraceFmt (RuntimeErr) in
  Fmt.fprintf fmt "%a%a%a" MsgFmt.pp err.msgs ErrSrcFmt.pp err.src
    (pp_opt RtTraceFmt.pp) err.trace

let str (err : t) = Fmt.asprintf "%a" pp err

let push (msg : msg) (exn : exn) : exn =
  match exn with
  | Error err -> Error { err with msgs = msg :: err.msgs }
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "trace error"))

let src (exn : exn) : ErrSrc.t =
  match exn with
  | Error err -> err.src
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "trace error"))

let set_src (src : ErrSrc.t) (exn : exn) : exn =
  match exn with
  | Error err -> Error { err with src }
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "trace error"))

let trace (exn : exn) : RtTrace.t option =
  match exn with
  | Error err -> err.trace
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "trace error"))

let set_trace (trace : RtTrace.t) (exn : exn) : exn =
  match exn with
  | Error err -> Error { err with trace = Some trace }
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "trace error"))

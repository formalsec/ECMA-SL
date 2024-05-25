open EslBase
open EslSyntax
module ErrSrc = Error_source
module RtTrace = Error_trace

type msg =
  | Default
  | Custom of string
  | Unexpected of string
  | UnexpectedRetval of Val.t
  | Failure of string
  | Unexpected of string
  | UncaughtExn of string
  | OpEvalExn of string
  | UnknownVar of Id.t'
  | UnknownFunc of Id.t'
  | MissingReturn of Id.t
  | BadNArgs of int * int
  | BadArg of string * Value.t
  | BadVal of string * Value.t
  | BadExpr of string * Value.t
  | BadFuncId of Value.t
  | BadOpArgs of string * Value.t list

module RuntimeErr : Error_type.ERROR_TYPE with type t = msg = struct
  type t = msg

  let header : string = "RuntimeError"
  let font : Font.t = [ Red ]

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') -> String.equal msg1' msg2'
    | (Unexpected msg1', Unexpected msg2') -> String.equal msg1' msg2'
    | (UnexpectedRetval v1, UnexpectedRetval v2) -> Val.equal v1 v2
    | (Failure msg1', Failure msg2') -> String.equal msg1' msg2'
    | (Unexpected msg1', Unexpected msg2') -> String.equal msg1' msg2'
    | (UncaughtExn msg1', UncaughtExn msg2') -> String.equal msg1' msg2'
    | (OpEvalExn oplbl1, OpEvalExn oplbl2) -> String.equal oplbl1 oplbl2
    | (UnknownVar x1, UnknownVar x2) -> String.equal x1 x2
    | (UnknownFunc fn1, UnknownFunc fn2) -> String.equal fn1 fn2
    | (MissingReturn fn1, MissingReturn fn2) -> Id.equal fn1 fn2
    | (BadNArgs (npxs1, nargs1), BadNArgs (npxs2, nargs2)) ->
      Int.equal npxs1 npxs2 && Int.equal nargs1 nargs2
    | (BadArg (texp1, v1), BadArg (texp2, v2)) ->
      String.equal texp1 texp2 && Value.equal v1 v2
    | (BadVal (texp1, v1), BadVal (texp2, v2)) ->
      String.equal texp1 texp2 && Value.equal v1 v2
    | (BadExpr (texp1, v1), BadExpr (texp2, v2)) ->
      String.equal texp1 texp2 && Value.equal v1 v2
    | (BadFuncId v1, BadFuncId v2) -> Value.equal v1 v2
    | _ -> false

  let pp (ppf : Fmt.t) (msg : t) : unit =
    let open Fmt in
    match msg with
    | Default -> fprintf fmt "Generic runtime error."
    | Custom msg' -> fprintf fmt "%s" msg'
    | Unexpected msg -> fprintf fmt "Unexpected %s." msg
    | UnexpectedRetval v -> fprintf fmt "Unexpected return value '%a'." Val.pp v
    | Failure msg -> fprintf fmt "Failure %s." msg
    | UncaughtExn msg -> fprintf fmt "Uncaught exception %s." msg
    | OpEvalErr oplbl -> fprintf fmt "Exception in Operator.%s." oplbl
    | UnknownVar x -> fprintf fmt "Cannot find variable '%s'." x
    | UnknownFunc fn -> fprintf fmt "Cannot find function '%s'." fn
    | BadNArgs (npxs, nargs) ->
      fmt ppf "Expected %d arguments, but got %d." npxs nargs
    | BadArg (texp, v) ->
      fmt ppf "Expecting argument of type '%s' but got '%a'." texp Value.pp v
    | BadVal (texp, v) ->
      fmt ppf "Expecting %s value, but got '%a'." texp Value.pp v
    | BadExpr (texp, v) ->
      fmt ppf "Expecting %s expression, but got '%a'." texp Value.pp v
    | BadFuncId v ->
      fmt ppf "Expecting a function identifier, but got '%a'." Value.pp v
    | BadOpArgs (texp, vs) when List.length vs = 1 ->
      fmt ppf "Expecting argument of type '%s', but got '%a'." texp
        (pp_lst !>", " Value.pp) vs
    | BadOpArgs (texp, vs) ->
      fmt ppf "Expecting arguments of types '%s', but got '(%a)'." texp
        (pp_lst !>", " Value.pp) vs

  let str (msg : t) : string = Fmt.str "%a" pp msg [@@inline]
end

type t =
  { msgs : msg list
  ; src : ErrSrc.t
  ; trace : RtTrace.t option
  }

exception Error of t

let raise (err : t) : 'a = Stdlib.raise_notrace (Error err) [@@inline]

let create ?(src : ErrSrc.t = Source.none) (msgs : msg list) : t =
  { msgs; src; trace = None }
[@@inline]

let throw ?(src : ErrSrc.t = Source.none) (msg : msg) : 'a =
  raise @@ create ~src [ msg ]
[@@inline]

let src (err : t) : ErrSrc.t = err.src [@@inline]
let trace (err : t) : RtTrace.t option = err.trace [@@inline]
let set_src (src : ErrSrc.t) (err : t) : t = { err with src } [@@inline]

let set_trace (tr : RtTrace.t) (err : t) : t = { err with trace = Some tr }
[@@inline]

let push (msg : msg) (err : t) : t = { err with msgs = msg :: err.msgs }
[@@inline]

let pp (ppf : Fmt.t) (err : t) : unit =
  let open Fmt in
  let module MsgFmt = Error_type.ErrorTypeFmt (RuntimeErr) in
  let module ErrSrcFmt = ErrSrc.ErrSrcFmt (RuntimeErr) in
  let module RtTraceFmt = RtTrace.RtTraceFmt (RuntimeErr) in
  Fmt.fmt ppf "%a%a%a" MsgFmt.pp err.msgs ErrSrcFmt.pp err.src
    (pp_opt RtTraceFmt.pp) err.trace

let str (err : t) = Fmt.str "%a" pp err [@@inline]

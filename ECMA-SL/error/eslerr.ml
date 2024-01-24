module ErrSrc = Eslerr_comp.ErrSrc
module RtTrace = Eslerr_comp.RtTrace

(* Error types *)
module InternalErr = Eslerr_type.Internal
module CompileErr = Eslerr_type.Compile
module RuntimeErr = Eslerr_type.Runtime

type interr = InternalErr.t
type comperr = CompileErr.t
type rterr = RuntimeErr.t

type internal =
  { msg : interr
  ; loc : string
  }

type compile =
  { msgs : comperr list
  ; src : ErrSrc.t
  }

type runtime =
  { msgs : rterr list
  ; src : ErrSrc.t
  ; trace : RtTrace.t option
  }

exception Internal_error of internal
exception Compile_error of compile
exception Runtime_error of runtime

(* Error generation *)

let internal' (msg : interr) (loc : string) : exn = Internal_error { msg; loc }
let internal (loc : string) (msg : interr) : 'a = internal' msg loc |> raise

let compile' ?(src : ErrSrc.t = ErrSrc.none ()) (msgs : comperr list) : exn =
  Compile_error { msgs; src }

let compile ?(src : ErrSrc.t = ErrSrc.none ()) (msg : comperr) : 'a =
  compile' [ msg ] ~src |> raise

let runtime' ?(src : ErrSrc.t = ErrSrc.none ()) (msgs : rterr list) : exn =
  Runtime_error { msgs; src; trace = None }

let runtime ?(src : ErrSrc.t = ErrSrc.none ()) (msg : rterr) : 'a =
  runtime' [ msg ] ~src |> raise

(* Error message update *)

let push_comp (msg : comperr) = function
  | Compile_error err -> Compile_error { err with msgs = msg :: err.msgs }
  | exn -> exn

let push_rt (msg : rterr) = function
  | Runtime_error err -> Runtime_error { err with msgs = msg :: err.msgs }
  | exn -> exn

(* Component functions *)

let src = function
  | Compile_error err -> err.src
  | Runtime_error err -> err.src
  | _ -> ErrSrc.none ()

let set_src (src : ErrSrc.t) = function
  | Compile_error err -> Compile_error { err with src }
  | Runtime_error err -> Runtime_error { err with src }
  | exn -> exn

let trace = function Runtime_error err -> err.trace | _ -> None

let set_trace (trace : RtTrace.t) = function
  | Runtime_error err -> Runtime_error { err with trace = Some trace }
  | exn -> exn

(* Formatting functions *)

let pp_int (fmt : Fmt.t) (err : internal) : unit =
  Fmt.fprintf fmt "(%s) %a" err.loc Eslerr_type.InternalFmt.pp err.msg

let pp_comp (fmt : Fmt.t) (err : compile) : unit =
  let module MsgFmt = Eslerr_fmt.Msgs (Eslerr_type.CompileFmt) in
  let module CodeFmt = Eslerr_fmt.Code (Eslerr_type.CompileFmt) in
  Fmt.fprintf fmt "%a%a" MsgFmt.pp err.msgs CodeFmt.pp err.src

let pp_rt (fmt : Fmt.t) (err : runtime) : unit =
  let module MsgFmt = Eslerr_fmt.Msgs (Eslerr_type.RuntimeFmt) in
  let module CodeFmt = Eslerr_fmt.Code (Eslerr_type.RuntimeFmt) in
  let module CustomFmt = Eslerr_fmt.Custom (Eslerr_type.RuntimeFmt) in
  Fmt.fprintf fmt "%a%a%a" MsgFmt.pp err.msgs CodeFmt.pp err.src
    CustomFmt.pp_trace err.trace

let pp (fmt : Fmt.t) = function
  | Internal_error err -> pp_int fmt err
  | Compile_error err -> pp_comp fmt err
  | Runtime_error err -> pp_rt fmt err
  | exn -> Fmt.fprintf fmt "%s" (Printexc.to_string exn)

let str (exn : exn) = Fmt.asprintf "%a" pp exn

(* Utility functions *)
let index_to_el (lst : 'a list) (src : ErrSrc.t) : 'a =
  match src with
  | Index i -> (
    try List.nth lst (i - 1)
    with _ -> internal __FUNCTION__ (Expecting "in-bound index") )
  | _ -> internal __FUNCTION__ (Expecting "index token")

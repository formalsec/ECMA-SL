module ErrSrc = Eslerr_comp.ErrSrc
module RtTrace = Eslerr_comp.RtTrace

(* Error types *)
type interr = Eslerr_type.internal
type comperr = Eslerr_type.compile
type rterr = Eslerr_type.runtime

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

let internal (loc : string) (msg : interr) : 'a =
  raise @@ Internal_error { msg; loc }

let compile ?(src : ErrSrc.t = ErrSrc.none ()) (msg : comperr) : 'a =
  raise @@ Compile_error { msgs = [ msg ]; src }

let runtime ?(src : ErrSrc.t = ErrSrc.none ()) (msg : rterr) : 'a =
  raise @@ Runtime_error { msgs = [ msg ]; src; trace = None }

(* Error message retrieval *)

let msg_int = function
  | Internal_error err -> err.msg
  | _ -> internal __FUNCTION__ (Expecting "internal error")

let msg_comp = function
  | Compile_error { msgs = msg :: _; _ } -> msg
  | Compile_error { msgs = []; _ } ->
    internal __FUNCTION__ (Expecting "non-empty error list")
  | _ -> internal __FUNCTION__ (Expecting "compile error")

let msg_rt = function
  | Runtime_error { msgs = msg :: _; _ } -> msg
  | Runtime_error { msgs = []; _ } ->
    internal __FUNCTION__ (Expecting "non-empty error list")
  | _ -> internal __FUNCTION__ (Expecting "runtime error")

let msg = function
  | Internal_error _ as exn -> Eslerr_type.Internal.str (msg_int exn)
  | Compile_error _ as exn -> Eslerr_type.Compile.str (msg_comp exn)
  | Runtime_error _ as exn -> Eslerr_type.Runtime.str (msg_rt exn)
  | exn -> Printexc.to_string_default exn

(* Error message update *)

let push_comp (msg : comperr) = function
  | Compile_error err -> Compile_error { err with msgs = msg :: err.msgs }
  | _ -> internal __FUNCTION__ (Expecting "compile error")

let push_rt (msg : rterr) = function
  | Runtime_error err -> Runtime_error { err with msgs = msg :: err.msgs }
  | _ -> internal __FUNCTION__ (Expecting "runtime error")

(* Component functions *)

let src = function
  | Compile_error err -> err.src
  | Runtime_error err -> err.src
  | _ -> internal __FUNCTION__ (Expecting "error type with source component")

let set_src (src : ErrSrc.t) = function
  | Compile_error err -> Compile_error { err with src }
  | Runtime_error err -> Runtime_error { err with src }
  | _ -> internal __FUNCTION__ (Expecting "error type with source component")

let trace = function
  | Runtime_error err -> err.trace
  | _ -> internal __FUNCTION__ (Expecting "error type with trace component")

let set_trace (trace : RtTrace.t) = function
  | Runtime_error err -> Runtime_error { err with trace = Some trace }
  | _ -> internal __FUNCTION__ (Expecting "error type with trace component")

(* Formatting functions *)

let pp_int (fmt : Fmt.t) (err : internal) : unit =
  Fmt.fprintf fmt "(%s) %a" err.loc Eslerr_type.Internal.pp err.msg

let pp_comp (fmt : Fmt.t) (err : compile) : unit =
  let module MsgFmt = Eslerr_fmt.Msgs (Eslerr_type.Compile) in
  let module CodeFmt = Eslerr_fmt.Code (Eslerr_type.Compile) in
  Fmt.fprintf fmt "%a%a" MsgFmt.pp err.msgs CodeFmt.pp err.src

let pp_rt (fmt : Fmt.t) (err : runtime) : unit =
  let module MsgFmt = Eslerr_fmt.Msgs (Eslerr_type.Runtime) in
  let module CodeFmt = Eslerr_fmt.Code (Eslerr_type.Runtime) in
  let module CustomFmt = Eslerr_fmt.Custom (Eslerr_type.Runtime) in
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

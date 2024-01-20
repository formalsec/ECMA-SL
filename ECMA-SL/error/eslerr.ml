(* Error types *)

module InternalErr = Eslerr_type.Internal
module CompileErr = Eslerr_type.Compile
module RuntimeErr = Eslerr_type.Runtime

type token = Eslerr_token.t
type interr = InternalErr.t
type comperr = CompileErr.t
type rterr = RuntimeErr.t

type internal =
  { loc : string
  ; msg : interr
  }

type compile =
  { loc : token
  ; src : token
  ; msgs : comperr list
  }

type runtime =
  { loc : token
  ; src : token
  ; msgs : rterr list
  ; trace : (Fmt.t -> unit -> unit) option
  }

exception Internal_error of internal
exception Compile_error of compile
exception Runtime_error of runtime

(* Error generation *)

let internal' (loc : string) (msg : interr) : exn = Internal_error { msg; loc }

let compile' ?(loc : token = NoTkn) ?(src : token = NoTkn) (msgs : comperr list)
  : exn =
  Compile_error { loc; src; msgs }

let runtime' ?(loc : token = NoTkn) ?(src : token = NoTkn) (msgs : rterr list) :
  exn =
  Runtime_error { loc; src; msgs; trace = None }

let internal (loc : string) (msg : interr) : 'a = internal' loc msg |> raise

let compile ?(loc : token = NoTkn) ?(src : token = NoTkn) (msg : comperr) : 'a =
  compile' ~loc ~src [ msg ] |> raise

let runtime ?(loc : token = NoTkn) ?(src : token = NoTkn) (msg : rterr) : 'a =
  runtime' ~loc ~src [ msg ] |> raise

(* Common functions *)

let loc = function Runtime_error err -> err.loc | _ -> NoTkn
let src = function Runtime_error err -> err.src | _ -> NoTkn
let trace = function Runtime_error err -> err.trace | _ -> None

let set_loc (loc : token) = function
  | Runtime_error err -> Runtime_error { err with loc }
  | exn -> exn

let set_src (src : token) = function
  | Runtime_error err -> Runtime_error { err with src }
  | exn -> exn

let set_trace (trace_pp : Fmt.t -> unit -> unit) = function
  | Runtime_error err -> Runtime_error { err with trace = Some trace_pp }
  | exn -> exn

(* Error message functions *)

let push_comp (msg : comperr) = function
  | Compile_error err -> Compile_error { err with msgs = msg :: err.msgs }
  | exn -> exn

let push_rt (msg : rterr) = function
  | Runtime_error err -> Runtime_error { err with msgs = msg :: err.msgs }
  | exn -> exn

(* Formatting functions *)

let pp_int (fmt : Fmt.t) (err : internal) : unit =
  Fmt.fprintf fmt "(%s) %a" err.loc Eslerr_type.InternalFmt.pp err.msg

let pp_comp (fmt : Fmt.t) (err : compile) : unit =
  let module MsgFmt = Eslerr_fmt.Msgs (Eslerr_type.CompileFmt) in
  let module CodeFmt = Eslerr_fmt.Code (Eslerr_type.CompileFmt) in
  Fmt.fprintf fmt "%a%a" MsgFmt.pp err.msgs CodeFmt.pp (err.loc, err.src)

let pp_rt (fmt : Fmt.t) (err : runtime) : unit =
  let module MsgFmt = Eslerr_fmt.Msgs (Eslerr_type.RuntimeFmt) in
  let module CodeFmt = Eslerr_fmt.Code (Eslerr_type.RuntimeFmt) in
  let module CustomFmt = Eslerr_fmt.Custom (Eslerr_type.RuntimeFmt) in
  Fmt.fprintf fmt "%a%a%a" MsgFmt.pp err.msgs CodeFmt.pp (err.loc, err.src)
    CustomFmt.pp_trace err.trace

let pp (fmt : Fmt.t) = function
  | Internal_error err -> pp_int fmt err
  | Compile_error err -> pp_comp fmt err
  | Runtime_error err -> pp_rt fmt err
  | exn -> Fmt.fprintf fmt "%s" (Printexc.to_string exn)

let str (exn : exn) = Fmt.asprintf "%a" pp exn

(* Utility functions *)

let index_to_el (lst : 'a list) (tkn : token) : 'a =
  match tkn with
  | Index i -> (
    try List.nth lst (i - 1)
    with _ -> internal __FUNCTION__ (Expecting "in-bound index") )
  | _ -> internal __FUNCTION__ (Expecting "index token")

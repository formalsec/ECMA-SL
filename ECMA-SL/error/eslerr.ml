(* Error types *)

module InternalErr = Eslerr_type.Internal
module RuntimeErr = Eslerr_type.Runtime

type token = Eslerr_token.t
type internal_msg = InternalErr.t
type runtime_msg = RuntimeErr.t

type internal =
  { loc : string
  ; msg : internal_msg
  }

type runtime =
  { loc : token
  ; src : token
  ; msgs : runtime_msg list
  ; trace : (Format.formatter -> unit -> unit) option
  }

exception Internal_error of internal
exception Runtime_error of runtime

(* Error generation *)

let internal' (loc : string) (msg : internal_msg) : exn =
  Internal_error { msg; loc }

let runtime' ?(loc : token = NoTkn) ?(src : token = NoTkn)
  (msgs : runtime_msg list) : exn =
  Runtime_error { loc; src; msgs; trace = None }

let internal (loc : string) (msg : internal_msg) : 'a =
  internal' loc msg |> raise

let runtime ?(loc : token = NoTkn) ?(src : token = NoTkn) (msg : runtime_msg) :
  'a =
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

let set_trace (trace_pp : Format.formatter -> unit -> unit) = function
  | Runtime_error err -> Runtime_error { err with trace = Some trace_pp }
  | exn -> exn

(* Runtime error specific functions *)
let push_rt (msg : runtime_msg) = function
  | Runtime_error err -> Runtime_error { err with msgs = msg :: err.msgs }
  | exn -> exn

(* Formatting functions *)

let internal_pp (fmt : Fmt.t) (err : internal) : unit =
  Fmt.fprintf fmt "(%s) %a" err.loc Eslerr_type.InternalFmt.pp err.msg

let runtime_pp (fmt : Fmt.t) (err : runtime) : unit =
  let module MsgFmt = Eslerr_fmt.Msgs (Eslerr_type.RuntimeFmt) in
  let module CodeFmt = Eslerr_fmt.Code (Eslerr_type.RuntimeFmt) in
  let module CustomFmt = Eslerr_fmt.Custom (Eslerr_type.RuntimeFmt) in
  Fmt.fprintf fmt "%a%a%a" MsgFmt.pp err.msgs CodeFmt.pp (err.loc, err.src)
    CustomFmt.pp_trace err.trace

let pp (fmt : Fmt.t) = function
  | Internal_error err -> internal_pp fmt err
  | Runtime_error err -> runtime_pp fmt err
  | exn -> Fmt.fprintf fmt "%s" (Printexc.to_string exn)

let str (exn : exn) = Fmt.asprintf "%a" pp exn

(* Utility functions *)

let index_to_el (lst : 'a list) (tkn : token) : 'a =
  match tkn with
  | Index i -> (
    try List.nth lst (i - 1)
    with _ -> internal __FUNCTION__ (Expecting "in-bound index") )
  | _ -> internal __FUNCTION__ (Expecting "index token")

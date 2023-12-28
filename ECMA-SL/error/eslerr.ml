open Eslerr_fmt

(* Error types *)

module InternalErr = Eslerr_msgs.Internal
module RuntimeErr = Eslerr_msgs.Runtime

type internal_err = InternalErr.t
type runtime_err = RuntimeErr.t

type internal =
  { loc : string
  ; msg : internal_err
  }

type runtime =
  { loc : token
  ; src : token
  ; msgs : runtime_err list
  }

exception Internal_error of internal
exception Runtime_error of runtime

(* Error generation *)

let internal' (loc : string) (msg : internal_err) : exn =
  Internal_error { msg; loc }

let runtime' ?(loc : token = NoTkn) ?(src : token = NoTkn)
  (msgs : runtime_err list) : exn =
  Runtime_error { loc; src; msgs }

let internal (loc : string) (msg : internal_err) : 'a =
  internal' loc msg |> raise

let runtime ?(loc : token = NoTkn) ?(src : token = NoTkn) (msg : runtime_err) :
  'a =
  runtime' ~loc ~src [ msg ] |> raise

(* Common functions *)

let loc = function Runtime_error err -> err.loc | _ -> NoTkn
let src = function Runtime_error err -> err.src | _ -> NoTkn

let set_loc (loc : token) = function
  | Runtime_error err -> Runtime_error { err with loc }
  | exn -> exn

let set_src (src : token) = function
  | Runtime_error err -> Runtime_error { err with src }
  | exn -> exn

(* Runtime error specific functions *)

let push_rt (msg : runtime_err) = function
  | Runtime_error err -> Runtime_error { err with msgs = msg :: err.msgs }
  | exn -> exn

(* Formatting functions *)

let format_int (err : internal) : string =
  let msg_str = Eslerr_msgs.internal_message_str err.msg in
  Printf.sprintf "(%s) %s" err.loc msg_str

let format_rt (err : runtime) : string =
  let (header, font) = ("RuntimeError", Font.red) in
  let msgs_str = List.map Eslerr_msgs.runtime_message_str err.msgs in
  let main_str = Eslerr_fmt.format_message font header msgs_str in
  let source_str = Eslerr_fmt.format_source font err.loc err.src in
  Printf.sprintf "%s%s" main_str source_str

let format = function
  | Internal_error err -> format_int err
  | Runtime_error err -> format_rt err
  | exn -> Printexc.to_string exn

(* Utility functions *)

let index_to_el (lst : 'a list) (tkn : token) : 'a =
  match tkn with
  | Index i -> (
    try List.nth lst (i - 1)
    with _ -> internal __FUNCTION__ (Expecting "in-bound index") )
  | _ -> internal __FUNCTION__ (Expecting "index token")

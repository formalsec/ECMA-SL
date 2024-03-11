open EslCore
module ErrSrc = Error_source

type msg =
  | Default
  | Custom of string
  | UnknownDependency of Id.t
  | CyclicDependency of Id.t
  | DuplicatedField of Id.t
  | DuplicatedTDef of Id.t
  | DuplicatedFunc of Id.t
  | DuplicatedMacro of Id.t
  | UnknownMacro of Id.t
  | BadNArgs of int * int
  | DuplicatedSwitchCase of Val.t
  | DuplicatedTField of Id.t
  | DuplicatedSigmaDiscriminant of EType.t
  | MissingSigmaDiscriminant of Id.t
  | UnexpectedSigmaDiscriminant
  | UnexpectedSigmaCase

module CompileErr : Error_type.ERROR_TYPE with type t = msg = struct
  type t = msg

  let header : string = "CompileError"
  let font : Font.t = [ Red ]

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') -> String.equal msg1' msg2'
    | (UnknownDependency file1, UnknownDependency file2) -> Id.equal file1 file2
    | (CyclicDependency file1, CyclicDependency file2) -> Id.equal file1 file2
    | (DuplicatedField fn1, DuplicatedField fn2) -> Id.equal fn1 fn2
    | (DuplicatedTDef tn1, DuplicatedTDef tn2) -> Id.equal tn1 tn2
    | (DuplicatedFunc fn1, DuplicatedFunc fn2) -> Id.equal fn1 fn2
    | (DuplicatedMacro mn1, DuplicatedMacro mn2) -> Id.equal mn1 mn2
    | (UnknownMacro mn1, UnknownMacro mn2) -> Id.equal mn1 mn2
    | (BadNArgs (npxs1, nargs1), BadNArgs (npxs2, nargs2)) ->
      Int.equal npxs1 npxs2 && Int.equal nargs1 nargs2
    | (DuplicatedSwitchCase v1, DuplicatedSwitchCase v2) -> Val.equal v1 v2
    | (DuplicatedTField fn1, DuplicatedTField fn2) -> Id.equal fn1 fn2
    | (DuplicatedSigmaDiscriminant lt1, DuplicatedSigmaDiscriminant lt2) ->
      EType.equal lt1 lt2
    | (MissingSigmaDiscriminant dsc1, MissingSigmaDiscriminant dsc2) ->
      Id.equal dsc1 dsc2
    | (UnexpectedSigmaDiscriminant, UnexpectedSigmaDiscriminant) -> true
    | (UnexpectedSigmaCase, UnexpectedSigmaCase) -> true
    | _ -> false

  let pp (fmt : Fmt.t) (msg : t) : unit =
    let open Fmt in
    match msg with
    | Default -> fprintf fmt "Generic compilation error."
    | Custom msg' -> fprintf fmt "%s" msg'
    | UnknownDependency file ->
      fprintf fmt "Cannot find dependency '%a'." Id.pp file
    | CyclicDependency file ->
      fprintf fmt "Cyclic dependency of file '%a'." Id.pp file
    | DuplicatedField fn ->
      fprintf fmt "Duplicated field name '%a' for object literal." Id.pp fn
    | DuplicatedTDef tn ->
      fprintf fmt "Duplicated definition for typedef '%a'." Id.pp tn
    | DuplicatedFunc fn ->
      fprintf fmt "Duplicated definition for function '%a'." Id.pp fn
    | DuplicatedMacro mn ->
      fprintf fmt "Duplicated definition for macro '%a'." Id.pp mn
    | UnknownMacro mn -> fprintf fmt "Cannot find macro '%a'." Id.pp mn
    | BadNArgs (npxs, nargs) ->
      fprintf fmt "Expected %d arguments, but got %d." npxs nargs
    | DuplicatedSwitchCase v ->
      fprintf fmt "Duplicated case value '%a' for switch statement." Val.pp v
    | DuplicatedTField fn ->
      fprintf fmt "Duplicated field name '%a' for object type." Id.pp fn
    | DuplicatedSigmaDiscriminant lt ->
      fprintf fmt "Duplicated discriminant '%a' for multiple sigma cases."
        EType.pp lt
    | MissingSigmaDiscriminant dsc ->
      fprintf fmt "Missing discriminant '%a' from sigma case." Id.pp dsc
    | UnexpectedSigmaDiscriminant ->
      fprintf fmt "Expecting literal type for sigma case discriminant."
    | UnexpectedSigmaCase ->
      fprintf fmt "Expecting union of object types for sigma type."

  let str (msg : t) : string = Fmt.asprintf "%a" pp msg
end

type t =
  { msgs : msg list
  ; src : ErrSrc.t
  }

exception Error of t

let create ?(src : ErrSrc.t = ErrSrc.none ()) (msgs : msg list) : exn =
  Error { msgs; src }

let throw ?(src : ErrSrc.t = ErrSrc.none ()) (msg : msg) : 'a =
  raise @@ create ~src [ msg ]

let pp (fmt : Fmt.t) (err : t) : unit =
  let module MsgFmt = Error_type.ErrorTypeFmt (CompileErr) in
  let module ErrSrcFmt = ErrSrc.ErrSrcFmt (CompileErr) in
  Fmt.fprintf fmt "%a%a" MsgFmt.pp err.msgs ErrSrcFmt.pp err.src

let str (err : t) = Fmt.asprintf "%a" pp err

let push (msg : msg) (exn : exn) : exn =
  match exn with
  | Error err -> Error { err with msgs = msg :: err.msgs }
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "compile error"))

let src (exn : exn) : ErrSrc.t =
  match exn with
  | Error err -> err.src
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "compile error"))

let set_src (src : ErrSrc.t) (exn : exn) : exn =
  match exn with
  | Error err -> Error { err with src }
  | _ -> Internal_error.(throw __FUNCTION__ (Expecting "compile error"))

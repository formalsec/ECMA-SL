open EslCore

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

type typing =
  | Default
  | Custom of string
  | BadCongruency of EType.t * EType.t
  | BadSubtyping of EType.t * EType.t
  | MissingField of Id.t
  | ExtraField of Id.t
  | IncompatibleField of Id.t
  | IncompatibleOptionalField of Id.t
  | IncompatibleSummaryField of Id.t
  | MissingSummaryField of EType.t
  | ExtraSummaryField
  | NExpectedElements of int * int
  | IncompatibleElement of int
  | IncompatibleSigmaDiscriminant
  | MissingSigmaCase of EType.t
  | ExtraSigmaCase of EType.t
  | IncompatibleSigmaCase of EType.t
  | MissingSigmaCaseDiscriminant of Id.t
  | UnknownSigmaCaseDiscriminant of EType.t

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

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') | (Expecting msg1', Expecting msg2') ->
      String.equal msg1' msg2'
    | (UnexpectedEval msg1', UnexpectedEval msg2')
    | (NotImplemented msg1', NotImplemented msg2') ->
      Option.equal String.equal msg1' msg2'
    | _ -> false

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
      fprintf fmt "Duplicated discriminant '%a' for multiple sigma type cases."
        EType.pp lt
    | MissingSigmaDiscriminant dsc ->
      fprintf fmt "Missing discriminant '%a' from the sigma type case." Id.pp
        dsc
    | UnexpectedSigmaDiscriminant ->
      fprintf fmt "Expecting literal type for sigma case discriminant."
    | UnexpectedSigmaCase ->
      fprintf fmt "Expecting union of object types for sigma type."

  let str (msg : t) : string = Fmt.asprintf "%a" pp msg
end

module Typing : ERR_TYPE with type t = typing = struct
  type t = typing

  let font () : Font.t = Font.Red
  let header () : string = "TypeError"

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') -> String.equal msg1' msg2'
    | (BadCongruency (tref1, tsrc1), BadCongruency (tref2, tsrc2))
    | (BadSubtyping (tref1, tsrc1), BadSubtyping (tref2, tsrc2)) ->
      EType.equal tref1 tref2 && EType.equal tsrc1 tsrc2
    | (MissingField fn1, MissingField fn2)
    | (ExtraField fn1, ExtraField fn2)
    | (IncompatibleField fn1, IncompatibleField fn2)
    | (IncompatibleOptionalField fn1, IncompatibleOptionalField fn2)
    | (IncompatibleSummaryField fn1, IncompatibleSummaryField fn2) ->
      Id.equal fn1 fn2
    | (MissingSummaryField ft1, MissingSummaryField ft2) -> EType.equal ft1 ft2
    | (ExtraSummaryField, ExtraSummaryField) -> true
    | ( NExpectedElements (ntsref1, ntssrc1)
      , NExpectedElements (ntsref2, ntssrc2) ) ->
      Int.equal ntsref1 ntsref2 && Int.equal ntssrc1 ntssrc2
    | (IncompatibleElement i1, IncompatibleElement i2) -> Int.equal i1 i2
    | (IncompatibleSigmaDiscriminant, IncompatibleSigmaDiscriminant) -> true
    | (MissingSigmaCase tdsc1, MissingSigmaCase tdsc2) ->
      EType.equal tdsc1 tdsc2
    | (ExtraSigmaCase tdsc1, ExtraSigmaCase tdsc2) -> EType.equal tdsc1 tdsc2
    | (IncompatibleSigmaCase tdsc1, IncompatibleSigmaCase tdsc2) ->
      EType.equal tdsc1 tdsc2
    | (MissingSigmaCaseDiscriminant dsc1, MissingSigmaCaseDiscriminant dsc2) ->
      Id.equal dsc1 dsc2
    | (UnknownSigmaCaseDiscriminant t1, UnknownSigmaCaseDiscriminant t2) ->
      EType.equal t1 t2
    | _ -> false

  let pp (fmt : Fmt.t) (msg : t) : unit =
    let open Fmt in
    match msg with
    | Default -> fprintf fmt "Generic type error."
    | Custom msg' -> fprintf fmt "%s" msg'
    | BadCongruency (tref, tsrc) ->
      fprintf fmt "Value of type '%a' is not congruent with type '%a'." EType.pp
        tsrc EType.pp tref
    | BadSubtyping (tref, tsrc) ->
      fprintf fmt "Value of type '%a' is not assignable to type '%a'." EType.pp
        tsrc EType.pp tref
    | MissingField fn ->
      fprintf fmt "Field '%a' is missing from the object's type." Id.pp fn
    | ExtraField fn ->
      fprintf fmt "Field '%a' is not defined in the object's type." Id.pp fn
    | IncompatibleField fn ->
      fprintf fmt "Types of field '%a' are incompatible." Id.pp fn
    | IncompatibleOptionalField fn ->
      fprintf fmt "Types of optional field '%a' are incompatible." Id.pp fn
    | IncompatibleSummaryField fn ->
      fprintf fmt "Type of field '%a' is incompatible with the summary type."
        Id.pp fn
    | MissingSummaryField ft ->
      fprintf fmt "Summary field '%a' is missing from the object's type."
        EType.pp ft
    | ExtraSummaryField ->
      fprintf fmt "Summary field is not defined in the object's type."
    | NExpectedElements (ntsref, ntssrc) ->
      fprintf fmt "Expecting %d elements, but %d were provided." ntsref ntssrc
    | IncompatibleElement i ->
      fprintf fmt "Types of the %s element are incompatible."
        (String_utils.ordinal_suffix i)
    | IncompatibleSigmaDiscriminant ->
      fprintf fmt "Discriminant fields are incompatible."
    | MissingSigmaCase tdsc ->
      fprintf fmt
        "Sigma case with discriminant '%a' is missing from the sigma type."
        EType.pp tdsc
    | ExtraSigmaCase tdsc ->
      fprintf fmt
        "Sigma case with discriminant '%a' is not defined in the sigma type."
        EType.pp tdsc
    | IncompatibleSigmaCase tdsc ->
      fprintf fmt "Sigma cases with discriminants '%a' are incompatible."
        EType.pp tdsc
    | MissingSigmaCaseDiscriminant dsc ->
      fprintf fmt "Missing discriminant '%a' from the sigma type case." Id.pp
        dsc
    | UnknownSigmaCaseDiscriminant tdsc ->
      fprintf fmt "Cannot find discriminant '%a' in the sigma type." EType.pp
        tdsc

  let str (msg : t) : string = Fmt.asprintf "%a" pp msg
end

module Runtime : ERR_TYPE with type t = runtime = struct
  type t = runtime

  let font () : Font.t = Font.Red
  let header () : string = "RuntimeError"

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

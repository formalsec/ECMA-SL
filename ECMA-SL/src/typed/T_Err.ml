open T_ErrFmt

type err_t =
  | UnknownVar of string
  | UnknownFunction of string
  | UnknownType of E_Type.t
  | NExpectedElements of int * int
  | NExpectedArgs of int * int
  | DuplicatedParam of string
  | DuplicatedField of string
  | MissingField of string
  | ExtraField of string
  | IncompatibleField of string
  | NoOverlapComp of E_Type.t * E_Type.t
  | BadValue of E_Type.t * E_Type.t
  | BadExpectedType of E_Type.t * E_Type.t
  | BadTypeUpdate of E_Type.t * E_Type.t
  | BadReturn of E_Type.t * E_Type.t
  | BadArgument of E_Type.t * E_Type.t
  | BadOperand of E_Type.t * E_Type.t
  | BadType of string option * E_Type.t
  | BadPossibleType of string option * E_Type.t
  | BadLookup of string * E_Type.t
  | BadSigma of E_Type.t
  | BadDiscriminant of string
  | MissingDiscriminant of string
  | UnknownDiscriminant of E_Type.t
  | DuplicatedPatternFld of string
  | BadValPattern of E_Type.t * E_Type.t
  | BadNonePattern
  | UnusedPatternCase
  | MissingPatternCase
  | MissingMainFunc
  | BadMainArgs
  | UnreachableCode
  | OpenCodePath

let err_str (err : err_t) : string =
  match err with
  | UnknownVar x -> Printf.sprintf "Cannot find variable '%s'." x
  | UnknownFunction fname -> Printf.sprintf "Cannot find function '%s'." fname
  | UnknownType t -> Printf.sprintf "Cannot find type '%s'." (E_Type.str t)
  | NExpectedElements (nsource, nelements) ->
      Printf.sprintf "Expecting %d elements, but %d elements were provided."
        nsource nelements
  | NExpectedArgs (nparams, nargs) ->
      Printf.sprintf "Expected %d arguments, but got %d." nparams nargs
  | DuplicatedParam param ->
      Printf.sprintf
        "Functions cannot have two parameters with the same name '%s'." param
  | DuplicatedField fn ->
      Printf.sprintf
        "Object literals cannot have two fields with the same name '%s'." fn
  | MissingField fn ->
      Printf.sprintf "Field '%s' is missing from the object's type." fn
  | ExtraField fn ->
      Printf.sprintf "Field '%s' is not defined in the object's type." fn
  | IncompatibleField fn ->
      Printf.sprintf "Types of field '%s' are incompatible." fn
  | NoOverlapComp (t1, t2) ->
      Printf.sprintf
        "This comparison appears to be unintentional because the types '%s' \
         and '%s' have no overlap."
        (E_Type.str t1) (E_Type.str t2)
  | BadValue (tref, texpr) ->
      Printf.sprintf "Value of type '%s' is not assignable to type '%s'."
        (E_Type.str texpr) (E_Type.str tref)
  | BadExpectedType (tref, texpr) ->
      Printf.sprintf
        "Expected value of type '%s' but a value of type '%s' was provided."
        (E_Type.str tref) (E_Type.str texpr)
  | BadTypeUpdate (tref, texpr) ->
      Printf.sprintf "Variable of type '%s' cannot change its type to '%s'."
        (E_Type.str tref) (E_Type.str texpr)
  | BadReturn (tret, texpr) ->
      Printf.sprintf "Value of type '%s' cannot be returned by a '%s' function."
        (E_Type.str texpr) (E_Type.str tret)
  | BadArgument (tparam, targ) ->
      Printf.sprintf
        "Argument of type '%s' is not assignable to a parameter of type '%s'."
        (E_Type.str targ) (E_Type.str tparam)
  | BadOperand (tparam, targ) ->
      Printf.sprintf
        "Argument of type '%s' is not assignable to a operand of type '%s'."
        (E_Type.str targ) (E_Type.str tparam)
  | BadType (x, t) ->
      let name = Option.default "Object" x in
      Printf.sprintf "'%s' is of type '%s'." name (E_Type.str t)
  | BadPossibleType (x, t) ->
      let name = Option.default "Object" x in
      Printf.sprintf "'%s' is possible of type '%s'." name (E_Type.str t)
  | BadLookup (fn, tobj) ->
      Printf.sprintf "Field '%s' does not exist on type '%s'." fn
        (E_Type.str tobj)
  | BadSigma texpr ->
      Printf.sprintf
        "Expecting sigma type value but a value of type '%s' was provided."
        (E_Type.str texpr)
  | BadDiscriminant d ->
      Printf.sprintf
        "Expecting a concrete value for the pattern-matching discriminant '%s'."
        d
  | MissingDiscriminant d ->
      Printf.sprintf
        "The discriminant '%s' is missing from the pattern-matching case." d
  | UnknownDiscriminant td ->
      Printf.sprintf
        "Discriminant '%s' is not associated with any pattern-matching case."
        (E_Type.str td)
  | DuplicatedPatternFld fn ->
      Printf.sprintf
        "Pattern-matching cases cannot have two fields with the same name '%s'."
        fn
  | BadValPattern (ft, pt) ->
      Printf.sprintf
        "A value pattern of type '%s' is not applicable to '%s' field."
        (E_Type.str pt) (E_Type.str ft)
  | BadNonePattern ->
      "The 'none' pattern cannot be applied to a non-optional field."
  | UnusedPatternCase -> "This pattern-matching case is unused."
  | MissingPatternCase ->
      "This pattern-matching is not exhaustive. The following case is missing:"
  | MissingMainFunc -> "The main function is missing from the program."
  | BadMainArgs -> "The main function is not expected to have any parameters."
  | UnreachableCode -> "Unreachable code detected."
  | OpenCodePath -> "Not all code paths return a value."

let error_kind () : kind_t = Error
let warning_kind () : kind_t = Warning
let no_tkn () : tkn_t = NoTkn
let lit_tkn (l : string) : tkn_t = Lit l
let str_tkn (s : string) : tkn_t = Str s
let type_tkn (t : E_Type.t) : tkn_t = Type t
let expr_tkn (expr : E_Expr.t) : tkn_t = Expr expr
let stmt_tkn (stmt : E_Stmt.t) : tkn_t = Stmt stmt
let func_tkn (func : E_Func.t) : tkn_t = Func func
let pat_tkn (pat : E_Pat.t) : tkn_t = Pat pat
let patval_tkn (patVal : E_Pat_v.t) : tkn_t = PatVal patVal

type t = { kind : kind_t; errs : err_t list; src : tkn_t; tkn : tkn_t }

exception TypeError of t

let top_err (terr : t) : err_t =
  match terr.errs with err :: _ -> err | [] -> failwith "T_Err.top_err"

let continue (terr : t) = raise (TypeError terr)

let create ?(kind : kind_t = Error) ?(src : tkn_t = NoTkn)
    ?(tkn : tkn_t = NoTkn) (err : err_t) : t =
  { kind; errs = [ err ]; src; tkn }

let push (terr : t) (err : err_t) =
  raise (TypeError { terr with errs = err :: terr.errs })

let update (terr : t) (err : err_t) =
  match terr.errs with
  | [] -> push terr err
  | _ :: errs' -> raise (TypeError { terr with errs = err :: errs' })

let raise ?(kind : kind_t = Error) ?(src : tkn_t = NoTkn) ?(tkn : tkn_t = NoTkn)
    (err : err_t) =
  raise (TypeError (create ~kind ~src ~tkn err))

let format (terr : t) : string =
  let terrMsgs = List.map err_str terr.errs in
  let terrMsg = T_ErrFmt.format_msg terr.kind terrMsgs in
  let terrSource = T_ErrFmt.format_source terr.kind terr.tkn terr.src in
  Printf.sprintf "%s%s\n" terrMsg terrSource

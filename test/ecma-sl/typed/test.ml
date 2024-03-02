open Ecma_sl
open EType

let ( ~@ ) (x : 'a) : 'a Source.phrase = Source.(x @> no_region)
let t_any : t = ~@AnyType
let t_unknown : t = ~@UnknownType
let t_never : t = ~@NeverType
let t_undefined : t = ~@UndefinedType
let t_null : t = ~@NullType
let t_void : t = ~@VoidType
let t_int : t = ~@IntType
let t_float : t = ~@FloatType
let t_string : t = ~@StringType
let t_boolean : t = ~@BooleanType
let t_symbol : t = ~@SymbolType
let lt_integer (i : int) : t = ~@(LiteralType (IntegerLit i))
let lt_float (f : float) : t = ~@(LiteralType (FloatLit f))
let lt_string (s : string) : t = ~@(LiteralType (StringLit s))
let lt_boolean (b : bool) : t = ~@(LiteralType (BooleanLit b))
let lt_symbol (s : string) : t = ~@(LiteralType (SymbolLit s))

let t_fld ?(opt : bool = false) (fn : Id.t') (ft : t) : tobjfld =
  (~@fn, ft, if opt then FldOpt else FldReq)

let t_obj ?(kind : tobjkind = ObjLit) ?(sfld : t option = None)
  (nflds : tobjfld list) : t =
  let flds nflds = function
    | Some tsmry -> (~@"*", tsmry, FldReq) :: nflds
    | None -> nflds
  in
  let flds = EParsing_helper.Type.parse_tobject (flds nflds sfld) in
  ~@(ObjectType { flds with kind })

let t_objlit ?sfld nflds = t_obj ~kind:ObjLit ?sfld nflds
let t_objsto ?sfld nflds = t_obj ~kind:ObjSto ?sfld nflds
let t_list (t : t) : t = ~@(ListType t)
let t_tuple (ts : t list) : t = ~@(TupleType ts)
let t_union (ts : t list) : t = ~@(UnionType ts)

let t_sigma (dsc : Id.t') (ts : t list) : t =
  ~@(SigmaType (~@dsc, EParsing_helper.Type.parse_tsigma ~@dsc ~@(UnionType ts)))

module Log = struct
  let expected (msg1 : string) (msg2 : string) : bool =
    Printf.printf "Expected: %s\nResult:   %s\n" msg1 msg2;
    false
end

module Syntax = struct
  let test (syntax : string) (expected : (t, Eslerr.comperr) Result.t) : bool =
    let module CompileErr = Eslerr_type.Compile in
    let result =
      try Ok (Parsing_utils.parse_etype syntax) with
      | Eslerr.Compile_error _ as exn -> Error (Eslerr.msg_comp exn)
      | exn -> raise exn
    in
    match (expected, result) with
    | (Ok t1, Ok t2) ->
      if equal t1 t2 then true else Log.expected (str t1) (str t2)
    | (Error err1, Error err2) ->
      if CompileErr.equal err1 err2 then true
      else Log.expected (CompileErr.str err1) (CompileErr.str err2)
    | (Ok t, Error err) -> Log.expected (str t) (CompileErr.str err)
    | (Error err, Ok t) -> Log.expected (CompileErr.str err) (str t)
end

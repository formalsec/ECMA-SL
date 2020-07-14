type t =
    IntType
  | FltType
  | StrType
  | BoolType

let str (v : t) : string = match v with
  | IntType  -> "__$int"
  | FltType  -> "__$float"
  | StrType  -> "__$string"
  | BoolType -> "__$boolean"

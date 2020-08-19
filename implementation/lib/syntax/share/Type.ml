type t =
  | UndefType
  | NullType
  | BoolType
  | StrType
  | NumberType
  | ObjType
  | ReferenceType
  | ListType
  | CompletionType
  | EnvironmentRecordType

let str (v : t) : string = match v with
  | UndefType             -> "__$undefined"
  | NullType              -> "__$null"
  | BoolType              -> "__$boolean"
  | StrType               -> "__$string"
  | NumberType            -> "__$number"
  | ObjType               -> "__$object"
  | ReferenceType         -> "__$reference"
  | ListType              -> "__$list"
  | CompletionType        -> "__$completion"
  | EnvironmentRecordType -> "__$environment_record"

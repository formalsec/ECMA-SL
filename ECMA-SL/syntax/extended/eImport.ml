open EslBase
open Source

type t = t' Source.t

and t' =
  | User of Id.t
  | Standard of Id.t

let default () : t = User (Id.default ()) @> none

let pp (ppf : Fmt.t) (import : t) : unit =
  match import.it with
  | User id -> Fmt.fmt ppf "import \"%a\";" Id.pp id
  | Standard id -> Fmt.fmt ppf "import %a;" Id.pp id

let str (import : t) : string = Fmt.str "%a" pp import

open Ecma_sl

type t =
  | Main
  | Latest
  | ECMARef5
  | ECMARef6
  | ECMARef6Sym

let all = [ Main; Latest; ECMARef5; ECMARef6 ]

let pp (fmt : Fmt.t) (version : t) : unit =
  match version with
  | Main -> Fmt.pp_str fmt "main"
  | Latest -> Fmt.pp_str fmt "latest"
  | ECMARef5 -> Fmt.pp_str fmt "ecmaref5"
  | ECMARef6 -> Fmt.pp_str fmt "ecmaref6"
  | ECMARef6Sym -> Fmt.pp_str fmt "ecmaref6-symbolic"

let str (version : t) : string = Fmt.asprintf "%a" pp version

let args (versions : t list) : (string * t) list =
  let to_arg = function
    | Main -> ("main", Main)
    | Latest -> ("latest", Latest)
    | ECMARef5 -> ("ecmaref5", ECMARef5)
    | ECMARef6 -> ("ecmaref6", ECMARef6)
    | ECMARef6Sym -> ("ecmaref6-sym", ECMARef6Sym)
  in
  List.map to_arg versions

let interp (version : t) : string =
  ( match version with
  | Main -> Share.es6_config ()
  | Latest -> Share.es6_config ()
  | ECMARef5 -> Share.es5_config ()
  | ECMARef6 -> Share.es6_config ()
  | ECMARef6Sym -> Share.es6_sym_config () )
  |> Option.get
  |> Share.resolve_config

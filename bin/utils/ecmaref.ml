type t =
  | Main
  | Latest
  | ECMARef5
  | ECMARef6

let all = [ Main; Latest; ECMARef5; ECMARef6 ]

let pp (fmt : Format.formatter) (version : t) : unit =
  let open Format in
  match version with
  | Main -> pp_print_string fmt "main"
  | Latest -> pp_print_string fmt "latest"
  | ECMARef5 -> pp_print_string fmt "ecmaref5"
  | ECMARef6 -> pp_print_string fmt "ecmaref6"

let args (versions : t list) : (string * t) list =
  let to_arg = function
    | Main -> ("main", Main)
    | Latest -> ("latest", Latest)
    | ECMARef5 -> ("ecmaref5", ECMARef5)
    | ECMARef6 -> ("ecmaref6", ECMARef6)
  in
  List.map to_arg versions

let interp (version : t) : string =
  Option.get
    ( match version with
    | Main -> Ecma_sl.Share.get_es6 ()
    | Latest -> Ecma_sl.Share.get_es6 ()
    | ECMARef5 -> Ecma_sl.Share.get_es5 ()
    | ECMARef6 -> Ecma_sl.Share.get_es6 () )

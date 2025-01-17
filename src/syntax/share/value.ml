open EslBase
include Smtml.Value

let void : t = Unit
let null : t = Nothing
let undefined : t = App (`Op "symbol", [ Str "undefined" ])
let loc (l : Loc.t) : t = App (`Op "loc", [ Int l ]) [@@inline]
let symbol (s : string) : t = App (`Op "symbol", [ Str s ]) [@@inline]

let is_special_number (s : string) : bool =
  List.mem s [ "nan"; "inf"; "-inf" ]
  || String.contains s 'e'
  || String.contains s 'E'

let float_str (f : float) : string =
  let f_str = Fmt.str "%.17g" f in
  if is_special_number f_str || String.contains f_str '.' then f_str
  else f_str ^ ".0"

let pp_custom_val (pp_v : t Fmt.t) (ppf : Format.formatter) (v : t) : unit =
  match v with
  | Unit -> ()
  | Nothing -> Fmt.string ppf "null"
  | Int i -> Fmt.int ppf i
  | Real f -> Fmt.string ppf (float_str f)
  | Str s -> Fmt.pf ppf "%S" s
  | True -> Fmt.string ppf "true"
  | False -> Fmt.string ppf "false"
  | List lst -> Fmt.(brackets (list ~sep:comma pp_v)) ppf lst
  | App (`Op "loc", [ Int loc ]) -> Loc.pp ppf loc
  | App (`Op "symbol", [ Str s ]) -> Fmt.pf ppf "'%s" s
  | App (`Op fn, fvs) -> Fmt.(pf ppf "{%S}@(%a)" fn (list ~sep:comma pp_v) fvs)
  | _ -> Log.fail "Val.pp_custom_val: unexpected value '%a'" pp v

let rec pp (ppf : Format.formatter) (v : t) : unit = pp_custom_val pp ppf v
let str (v : t) : string = Fmt.str "%a" pp v [@@inline]

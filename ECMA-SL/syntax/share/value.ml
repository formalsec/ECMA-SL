include Smtml.Value
open EslBase

let is_special_number (s : string) : bool =
  List.mem s [ "nan"; "inf"; "-inf" ]
  || String.contains s 'e'
  || String.contains s 'E'

let float_str (f : float) : string =
  let f_str = Fmt.str "%.17g" f in
  if is_special_number f_str || String.contains f_str '.' then f_str
  else f_str ^ ".0"

let pp_custom_val (pp_v : Fmt.t -> t -> unit) (ppf : Fmt.t) (v : t) : unit =
  match v with
  | Int i -> Fmt.format ppf "%i" i
  | Real f -> Fmt.format ppf "%s" (float_str f)
  | Str s -> Fmt.format ppf "%S" s
  | True -> Fmt.format ppf "true"
  | False -> Fmt.format ppf "false"
  | List lst -> Fmt.(format ppf "[%a]" (pp_lst !>", " pp_v) lst)
  | App (`Op "void", []) -> ()
  | App (`Op "null", []) -> Fmt.format ppf "null"
  | App (`Op "loc", [ Int l ]) -> Loc.pp ppf l
  | App (`Op "symbol", [ Str s ]) -> Fmt.format ppf "'%s" s
  | App (`Op fn, fvs) -> Fmt.(format ppf "{%S}@(%a)" fn (pp_lst !>", " pp_v) fvs)
  | _ -> Log.fail "Val.pp_custom_val: unexpected value '%a'" pp v

let rec pp (ppf : Fmt.t) (v : t) : unit = pp_custom_val pp ppf v
let str (v : t) : string = Fmt.str "%a" pp v

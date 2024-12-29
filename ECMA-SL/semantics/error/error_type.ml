open EslBase

module type ERROR_TYPE = sig
  type t

  val header : string
  val font : Font.t
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val str : t -> string
end

module ErrorTypeFmt (ErrorType : ERROR_TYPE) = struct
  let pp_cause (ppf : Format.formatter) (msg : ErrorType.t) : unit =
    let pp_font = Font.pp_text_err (Faint :: ErrorType.font) in
    Fmt.pf ppf "\n%a %a" pp_font "Caused by:" ErrorType.pp msg

  let pp_msgs (ppf : Format.formatter) (msgs : ErrorType.t list) : unit =
    match msgs with
    | [] -> Log.fail "expecting non-empty error message list"
    | main :: causes ->
      Fmt.pf ppf "%a%a" ErrorType.pp main Fmt.(list pp_cause) causes

  let pp (ppf : Format.formatter) (msgs : ErrorType.t list) : unit =
    let header = ErrorType.header ^ ":" in
    let font = ErrorType.font in
    Fmt.pf ppf "%a %a" (Font.pp_text_err font) header pp_msgs msgs

  let str (msgs : ErrorType.t list) : string = Fmt.str "%a" pp msgs [@@inline]
end

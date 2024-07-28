open EslBase

module type ERROR_TYPE = sig
  type t

  val header : string
  val font : Font.t
  val equal : t -> t -> bool
  val pp : Fmt.t -> t -> unit
  val str : t -> string
end

module ErrorTypeFmt (ErrorType : ERROR_TYPE) = struct
  let pp_cause (ppf : Fmt.t) (msg : ErrorType.t) : unit =
    let pp_font = Font.pp_text_err (Faint :: ErrorType.font) in
    Fmt.fmt ppf "\n%a %a" pp_font "Caused by:" ErrorType.pp msg

  let pp_msgs (ppf : Fmt.t) (msgs : ErrorType.t list) : unit =
    match msgs with
    | [] -> Log.fail "expecting non-empty error message list"
    | main :: causes ->
      Fmt.fmt ppf "%a%a" ErrorType.pp main Fmt.(pp_lst !>"" pp_cause) causes

  let pp (ppf : Fmt.t) (msgs : ErrorType.t list) : unit =
    let header = ErrorType.header ^ ":" in
    let font = ErrorType.font in
    Fmt.fmt ppf "%a %a" (Font.pp_text_err font) header pp_msgs msgs

  let str (msgs : ErrorType.t list) : string = Fmt.str "%a" pp msgs [@@inline]
end

module type ERROR_TYPE = sig
  type t

  val header : string
  val font : Font.t
  val equal : t -> t -> bool
  val pp : Fmt.t -> t -> unit
  val str : t -> string
end

module ErrorTypeFmt (ErrorType : ERROR_TYPE) = struct
  let pp_cause (fmt : Fmt.t) (msg : ErrorType.t) : unit =
    Fmt.fprintf fmt "\n%a %a"
      (Font.pp_text_err (Faint :: ErrorType.font))
      "Caused by:" ErrorType.pp msg

  let pp_msgs (fmt : Fmt.t) (msgs : ErrorType.t list) : unit =
    let open Fmt in
    match msgs with
    | [] -> Font.pp_text_err ErrorType.font fmt "???"
    | main :: causes ->
      fprintf fmt "%a%a" ErrorType.pp main (pp_lst "" pp_cause) causes

  let pp (fmt : Fmt.t) (msgs : ErrorType.t list) : unit =
    let header = ErrorType.header ^ ":" in
    let font = ErrorType.font in
    Fmt.fprintf fmt "\n%a %a" (Font.pp_text_err font) header pp_msgs msgs

  let str (msgs : ErrorType.t list) : string = Fmt.asprintf "%a" pp msgs
end

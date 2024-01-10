type t =
  | Auto
  | JS
  | ESL
  | CESL

let auto = ("auto", Auto)
let js = ("js", JS)
let esl = ("esl", ESL)
let cesl = ("cesl", CESL)

let pp (fmt : Format.formatter) (lang : t) : unit =
  let open Format in
  match lang with
  | Auto -> pp_print_string fmt "auto"
  | JS -> pp_print_string fmt ".js"
  | ESL -> pp_print_string fmt ".esl"
  | CESL -> pp_print_string fmt ".cesl"

let valid (langs : t list) (user_lang : t) : t list =
  match user_lang with Auto -> langs | _ -> [ user_lang ]

let of_file (ext : string) (langs : t list) : t option =
  match ext with
  | ".js" when List.mem JS langs -> Some JS
  | ".esl" when List.mem ESL langs -> Some ESL
  | ".cesl" when List.mem CESL langs -> Some CESL
  | _ -> None

type t =
  | Auto
  | JS
  | ESL
  | CESL
  | CESLUnattached

let pp (fmt : Format.formatter) (lang : t) : unit =
  let open Format in
  match lang with
  | Auto -> pp_print_string fmt "auto"
  | JS -> pp_print_string fmt ".js"
  | ESL -> pp_print_string fmt ".esl"
  | CESL -> pp_print_string fmt ".cesl"
  | CESLUnattached -> pp_print_string fmt ".cesl"

let args (langs : t list) : (string * t) list =
  let to_arg = function
    | Auto as lang -> ("auto", lang)
    | JS as lang -> ("js", lang)
    | ESL as lang -> ("esl", lang)
    | CESL as lang -> ("cesl", lang)
    | CESLUnattached as lang -> ("cesl-unattached", lang)
  in
  List.map to_arg langs

let valid (langs : t list) (user_lang : t) : t list =
  match user_lang with Auto -> langs | _ -> [ user_lang ]

let test_file_ext (langs : t list) (ext : string) : t option =
  match ext with
  | ".js" when List.mem JS langs -> Some JS
  | ".esl" when List.mem ESL langs -> Some ESL
  | ".cesl" when List.mem CESL langs -> Some CESL
  | ".cesl" when List.mem CESLUnattached langs -> Some CESLUnattached
  | _ -> None

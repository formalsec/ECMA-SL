open Ecma_sl

type t =
  | Auto
  | JS
  | ESL
  | CESL
  | CESLUnattached

let pp (fmt : Fmt.t) (lang : t) : unit =
  match lang with
  | Auto -> Fmt.pp_str fmt "auto"
  | JS -> Fmt.pp_str fmt ".js"
  | ESL -> Fmt.pp_str fmt ".esl"
  | CESL -> Fmt.pp_str fmt ".cesl"
  | CESLUnattached -> Fmt.pp_str fmt ".cesl"

let str (lang : t) : string = Fmt.asprintf "%a" pp lang

let args (langs : t list) : (string * t) list =
  let to_arg = function
    | Auto as lang -> ("auto", lang)
    | JS as lang -> ("js", lang)
    | ESL as lang -> ("esl", lang)
    | CESL as lang -> ("cesl", lang)
    | CESLUnattached as lang -> ("cesl-unattached", lang)
  in
  List.map to_arg langs

let valid_langs (valid_langs : t list) (user_lang : t) : t list =
  match user_lang with Auto -> valid_langs | _ -> [ user_lang ]

let resolve_file_ext (valid_langs : t list) (fpath : Fpath.t) : t option =
  match Fpath.get_ext fpath with
  | ".js" when List.mem JS valid_langs -> Some JS
  | ".esl" when List.mem ESL valid_langs -> Some ESL
  | ".cesl" when List.mem CESL valid_langs -> Some CESL
  | ".cesl" when List.mem CESLUnattached valid_langs -> Some CESLUnattached
  | _ -> None

let resolve_file_lang (valid_langs : t list) (fpath : Fpath.t) : t option =
  match resolve_file_ext valid_langs fpath with
  | Some _ as lang -> lang
  | None ->
    Log.warn "expecting file extensions: %a" (Fmt.pp_lst " | " pp) valid_langs;
    None

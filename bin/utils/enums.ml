module Lang = struct
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
end

module ECMARef = struct
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
end

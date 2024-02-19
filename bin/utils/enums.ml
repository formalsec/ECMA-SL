open Ecma_sl

module DebugLvl = struct
  type t =
    | None
    | Warn
    | Full

  let all = [ None; Warn; Full ]

  let pp (fmt : Fmt.t) (level : t) : unit =
    match level with
    | None -> Fmt.pp_str fmt "none"
    | Warn -> Fmt.pp_str fmt "warn"
    | Full -> Fmt.pp_str fmt "full"

  let args (levels : t list) : (string * t) list =
    let to_arg = function
      | None as level -> ("none", level)
      | Warn as level -> ("warn", level)
      | Full as level -> ("full", level)
    in
    List.map to_arg levels

  let value (level : t) : int =
    match level with None -> 0 | Warn -> 1 | Full -> 2
end

module Lang = struct
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

  let resolve_file_ext (langs : t list) (ext : string) : t option =
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

  let pp (fmt : Fmt.t) (version : t) : unit =
    match version with
    | Main -> Fmt.pp_str fmt "main"
    | Latest -> Fmt.pp_str fmt "latest"
    | ECMARef5 -> Fmt.pp_str fmt "ecmaref5"
    | ECMARef6 -> Fmt.pp_str fmt "ecmaref6"

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
      | Main -> Share.get_es6 ()
      | Latest -> Share.get_es6 ()
      | ECMARef5 -> Share.get_es5 ()
      | ECMARef6 -> Share.get_es6 () )
end

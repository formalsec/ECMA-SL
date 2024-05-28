open Ecma_sl

module DebugLvl = struct
  type t =
    | None
    | Warn
    | Full

  let all () : t list = [ None; Warn; Full ]

  let pp (ppf : Fmt.t) (lvl : t) : unit =
    match lvl with
    | None -> Fmt.pp_str ppf "none"
    | Warn -> Fmt.pp_str ppf "warn"
    | Full -> Fmt.pp_str ppf "full"

  let str (lvl : t) : string = Fmt.str "%a" pp lvl

  let args (lvls : t list) : (string * t) list =
    List.map (fun lvl -> (str lvl, lvl)) lvls

  let value (lvl : t) : int = match lvl with None -> 0 | Warn -> 1 | Full -> 2
  let ( < ) (lvl1 : t) (lvl2 : t) : bool = Stdlib.(value lvl1 < value lvl2)
  let ( > ) (lvl1 : t) (lvl2 : t) : bool = Stdlib.(value lvl1 > value lvl2)
  let ( <= ) (lvl1 : t) (lvl2 : t) : bool = Stdlib.(value lvl1 <= value lvl2)
  let ( >= ) (lvl1 : t) (lvl2 : t) : bool = Stdlib.(value lvl1 >= value lvl2)
end

module Lang = struct
  type t =
    | Auto
    | JS
    | ESL
    | CESL
    | CESLUnattached
    | TestReport
    | TestSummary

  let pp (ppf : Fmt.t) (lang : t) : unit =
    match lang with
    | Auto -> Fmt.pp_str ppf "auto"
    | JS -> Fmt.pp_str ppf ".js"
    | ESL -> Fmt.pp_str ppf ".esl"
    | CESL -> Fmt.pp_str ppf ".cesl"
    | CESLUnattached -> Fmt.pp_str ppf ".cesl"
    | TestReport -> Fmt.pp_str ppf ".trp"
    | TestSummary -> Fmt.pp_str ppf ".tsmry"

  let str (lang : t) : string = Fmt.str "%a" pp lang

  let description (lang : t) : string =
    match lang with
    | Auto -> "auto"
    | JS -> "js"
    | ESL -> "esl"
    | CESL -> "cesl"
    | CESLUnattached -> "cesl-unattached"
    | TestReport -> "trp"
    | TestSummary -> "tsmry"

  let args (langs : t list) : (string * t) list =
    List.map (fun lang -> (description lang, lang)) langs

  let valid_langs (valid_langs : t list) (user_lang : t) : t list =
    match user_lang with Auto -> valid_langs | _ -> [ user_lang ]

  let resolve_file_ext (valid_langs : t list) (fpath : Fpath.t) : t option =
    match Fpath.get_ext fpath with
    | ".js" when List.mem JS valid_langs -> Some JS
    | ".esl" when List.mem ESL valid_langs -> Some ESL
    | ".cesl" when List.mem CESL valid_langs -> Some CESL
    | ".cesl" when List.mem CESLUnattached valid_langs -> Some CESLUnattached
    | ".trp" when List.mem TestReport valid_langs -> Some TestReport
    | ".tsmry" when List.mem TestSummary valid_langs -> Some TestSummary
    | _ -> None

  let resolve_file_lang ?(warn : bool = true) (langs : t list) (fpath : Fpath.t)
    : t option =
    let lang = resolve_file_ext langs fpath in
    if Option.is_none lang && warn then
      Log.warn "expecting file extensions: %a" Fmt.(pp_lst !>" | " pp) langs;
    lang
end

module InterpTracer = struct
  type t =
    | None
    | Call
    | Step
    | Full
    | Core

  let all () : t list = [ None; Call; Step; Full; Core ]

  let pp (ppf : Fmt.t) (tracer : t) : unit =
    match tracer with
    | None -> Fmt.pp_str ppf "none"
    | Call -> Fmt.pp_str ppf "call"
    | Step -> Fmt.pp_str ppf "step"
    | Full -> Fmt.pp_str ppf "full"
    | Core -> Fmt.pp_str ppf "core"

  let str (tracer : t) : string = Fmt.str "%a" pp tracer

  let args (tracers : t list) : (string * t) list =
    List.map (fun tracer -> (str tracer, tracer)) tracers
end

module InterpProfiler = struct
  type t =
    | None
    | Time
    | Full

  let all () : t list = [ None; Time; Full ]

  let pp (ppf : Fmt.t) (profiler : t) : unit =
    match profiler with
    | None -> Fmt.pp_str ppf "none"
    | Time -> Fmt.pp_str ppf "time"
    | Full -> Fmt.pp_str ppf "full"

  let str (profiler : t) : string = Fmt.str "%a" pp profiler

  let args (profilers : t list) : (string * t) list =
    List.map (fun profiler -> (str profiler, profiler)) profilers
end

module JSInterp = struct
  type t =
    | Main
    | Latest
    | ECMARef5
    | ECMARef6
    | ECMARef6Sym

  let all () : t list = [ Main; Latest; ECMARef5; ECMARef6 ]

  let pp (ppf : Fmt.t) (version : t) : unit =
    match version with
    | Main -> Fmt.pp_str ppf "main"
    | Latest -> Fmt.pp_str ppf "latest"
    | ECMARef5 -> Fmt.pp_str ppf "ecmaref5"
    | ECMARef6 -> Fmt.pp_str ppf "ecmaref6"
    | ECMARef6Sym -> Fmt.pp_str ppf "ecmaref6-sym"

  let str (version : t) : string = Fmt.str "%a" pp version

  let args (versions : t list) : (string * t) list =
    List.map (fun version -> (str version, version)) versions

  let interp (version : t) : string =
    match version with
    | Main -> Share.es6_config ()
    | Latest -> Share.es6_config ()
    | ECMARef5 -> Share.es5_config ()
    | ECMARef6 -> Share.es6_config ()
    | ECMARef6Sym -> Share.es6_sym_config ()
end

module JSTest = struct
  type t =
    | Auto
    | Simple
    | Test262

  let all () : t list = [ Auto; Simple; Test262 ]

  let pp (ppf : Fmt.t) (kind : t) : unit =
    match kind with
    | Auto -> Fmt.pp_str ppf "auto"
    | Simple -> Fmt.pp_str ppf "simple"
    | Test262 -> Fmt.pp_str ppf "test262"

  let str (kind : t) : string = Fmt.str "%a" pp kind

  let args (kinds : t list) : (string * t) list =
    List.map (fun kind -> (str kind, kind)) kinds
end

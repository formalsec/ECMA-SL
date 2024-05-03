open Ecma_sl

module DebugLvl = struct
  type t =
    | None
    | Warn
    | Full

  let all () : t list = [ None; Warn; Full ]

  let pp (fmt : Fmt.t) (lvl : t) : unit =
    match lvl with
    | None -> Fmt.pp_str fmt "none"
    | Warn -> Fmt.pp_str fmt "warn"
    | Full -> Fmt.pp_str fmt "full"

  let str (lvl : t) : string = Fmt.asprintf "%a" pp lvl

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

  let pp (fmt : Fmt.t) (lang : t) : unit =
    match lang with
    | Auto -> Fmt.pp_str fmt "auto"
    | JS -> Fmt.pp_str fmt ".js"
    | ESL -> Fmt.pp_str fmt ".esl"
    | CESL -> Fmt.pp_str fmt ".cesl"
    | CESLUnattached -> Fmt.pp_str fmt ".cesl"

  let str (lang : t) : string = Fmt.asprintf "%a" pp lang

  let description (lang : t) : string =
    match lang with
    | Auto -> "auto"
    | JS -> "js"
    | ESL -> "esl"
    | CESL -> "cesl"
    | CESLUnattached -> "cesl-unattached"

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
    | _ -> None

  let resolve_file_lang (valid_langs : t list) (fpath : Fpath.t) : t option =
    let lang = resolve_file_ext valid_langs fpath in
    if Option.is_none lang then
      Log.warn "expecting file extensions: %a" (Fmt.pp_lst " | " pp) valid_langs;
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

  let pp (fmt : Fmt.t) (tracer : t) : unit =
    match tracer with
    | None -> Fmt.pp_str fmt "none"
    | Call -> Fmt.pp_str fmt "call"
    | Step -> Fmt.pp_str fmt "step"
    | Full -> Fmt.pp_str fmt "full"
    | Core -> Fmt.pp_str fmt "core"

  let str (tracer : t) : string = Fmt.asprintf "%a" pp tracer

  let args (tracers : t list) : (string * t) list =
    List.map (fun tracer -> (str tracer, tracer)) tracers
end

module InterpProfiler = struct
  type t =
    | None
    | Time
    | Full

  let all () : t list = [ None; Time; Full ]

  let pp (fmt : Fmt.t) (profiler : t) : unit =
    match profiler with
    | None -> Fmt.pp_str fmt "none"
    | Time -> Fmt.pp_str fmt "time"
    | Full -> Fmt.pp_str fmt "full"

  let str (profiler : t) : string = Fmt.asprintf "%a" pp profiler

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

  let pp (fmt : Fmt.t) (version : t) : unit =
    match version with
    | Main -> Fmt.pp_str fmt "main"
    | Latest -> Fmt.pp_str fmt "latest"
    | ECMARef5 -> Fmt.pp_str fmt "ecmaref5"
    | ECMARef6 -> Fmt.pp_str fmt "ecmaref6"
    | ECMARef6Sym -> Fmt.pp_str fmt "ecmaref6-sym"

  let str (version : t) : string = Fmt.asprintf "%a" pp version

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

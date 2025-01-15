type t' =
  | Before of Id.t
  | After of Id.t

type t = (Id.t * t') Source.t

let before (fn : Id.t) (an : Id.t) : Id.t * t' = (fn, Before an) [@@inline]
let after (fn : Id.t) (an : Id.t) : Id.t * t' = (fn, After an) [@@inline]
let fname (advice : t) : Id.t = fst advice.it [@@inline]

let aname (advice : t) : Id.t =
  match snd advice.it with Before an | After an -> an

let is_before (advice : t) : bool =
  match advice.it with (_, Before _) -> true | _ -> false

let is_after (advice : t) : bool =
  match advice.it with (_, After _) -> true | _ -> false

let pp (ppf : Format.formatter) (advice : (Id.t * t') Source.t) : unit =
  match advice.it with
  | (fn, Before an) -> Fmt.pf ppf "advice %a before %a;" Id.pp an Id.pp fn
  | (fn, After an) -> Fmt.pf ppf "advice %a after %a;" Id.pp an Id.pp fn

let str (advices : t) : string = Format.asprintf "%a" pp advices [@@inline]

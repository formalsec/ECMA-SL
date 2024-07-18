open EslBase
open Source

type t = t' Source.t

and t' =
  { name : Id.t
  ; tparams : (Id.t * EType.t option) list
  ; treturn : EType.t option
  ; body : EStmt.t
  }

let default : unit -> t =
  let (name, body) = (Id.default (), EStmt.default ()) in
  let (tparams, treturn) = ([], None) in
  let dflt = { name; tparams; treturn; body } @> none in
  fun () -> dflt

let create (name : Id.t) (tparams : (Id.t * EType.t option) list)
  (treturn : EType.t option) (body : EStmt.t) : t' =
  { name; tparams; treturn; body }
[@@inline]

let name (func : t) : Id.t = func.it.name [@@inline]
let name' (func : t) : Id.t' = (name func).it
let tparams (func : t) : (Id.t * EType.t option) list = func.it.tparams
let params (func : t) : Id.t list = List.map (fun (px, _) -> px) func.it.tparams
let params' (func : t) : Id.t' list = List.map (fun px -> px.it) (params func)
let treturn (func : t) : EType.t option = func.it.treturn [@@inline]
let body (func : t) : EStmt.t = func.it.body [@@inline]

let pp_signature (ppf : Fmt.t) (func : t) : unit =
  let pp_param ppf (px, t) = Fmt.fmt ppf "%a%a" Id.pp px EType.tannot_pp t in
  let pp_params ppf tpxs = Fmt.(pp_lst !>", " pp_param) ppf tpxs in
  Fmt.fmt ppf "function %a(%a)%a" Id.pp func.it.name pp_params func.it.tparams
    EType.tannot_pp func.it.treturn

let pp_simple (ppf : Fmt.t) (func : t) : unit =
  Fmt.fmt ppf "%a {..." pp_signature func

let pp (ppf : Fmt.t) (func : t) : unit =
  Fmt.fmt ppf "%a %a" pp_signature func EStmt.pp func.it.body

let str (func : t) : string = Fmt.str "%a" pp func [@@inline]

let lambdas (f : t) : (at * Id.t' * Id.t list * Id.t list * EStmt.t) list =
  let to_list_f s =
    match s.it with
    | EStmt.Lambda (_, id, pxs, ctxvars, s) -> [ (s.at, id, pxs, ctxvars, s) ]
    | _ -> []
  in
  EStmt.to_list ~recursion:true to_list_f (body f)

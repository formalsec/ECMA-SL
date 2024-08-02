open EslBase
open Source

type t = t' Source.t

and t' =
  { fn : Id.t
  ; tpxs : (Id.t * EType.t option) list
  ; tret : EType.t option
  ; s : EStmt.t
  }

let default : unit -> t =
  let (fn, s) = (Id.default (), EStmt.default ()) in
  let dflt = { fn; tpxs = []; tret = None; s } @> none in
  fun () -> dflt

let create (fn : Id.t) (tpxs : (Id.t * EType.t option) list)
  (tret : EType.t option) (s : EStmt.t) : t' =
  { fn; tpxs; tret; s }
[@@inline]

let name (f : t) : Id.t = f.it.fn [@@inline]
let name' (f : t) : Id.t' = (name f).it
let tparams (f : t) : (Id.t * EType.t option) list = f.it.tpxs [@@inline]
let params (f : t) : Id.t list = List.map (fun (px, _) -> px) (tparams f)
let params' (f : t) : Id.t' list = List.map (fun (px, _) -> px.it) (tparams f)
let treturn (f : t) : EType.t option = f.it.tret [@@inline]
let body (f : t) : EStmt.t = f.it.s [@@inline]

let pp_signature (ppf : Fmt.t) (f : t) : unit =
  let pp_tpx ppf (px, t) = Fmt.fmt ppf "%a%a" Id.pp px EType.tannot_pp t in
  let pp_tpxs ppf tpxs = Fmt.(pp_lst !>", " pp_tpx) ppf tpxs in
  Fmt.fmt ppf "function %a(%a)%a" Id.pp f.it.fn pp_tpxs f.it.tpxs
    EType.tannot_pp f.it.tret

let pp_simple (ppf : Fmt.t) (f : t) : unit =
  Fmt.fmt ppf "%a {..." pp_signature f

let pp (ppf : Fmt.t) (f : t) : unit =
  Fmt.fmt ppf "%a %a" pp_signature f EStmt.pp f.it.s

let str (f : t) : string = Fmt.str "%a" pp f [@@inline]

let lambdas (f : t) : (at * Id.t' * Id.t list * Id.t list * EStmt.t) list =
  let to_list_f s =
    match s.it with
    | EStmt.Lambda (_, id, pxs, ctxvars, s') -> [ (s.at, id, pxs, ctxvars, s') ]
    | _ -> []
  in
  EStmt.to_list ~recursive:true to_list_f (body f)

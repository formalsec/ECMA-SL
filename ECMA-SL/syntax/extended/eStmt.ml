open Source

type metadata_t =
  { where : string
  ; html : string
  }

type t = t' Source.phrase

and t' =
  | Skip
  | Debug of t
  | Block of t list
  | Print of EExpr.t
  | Return of EExpr.t
  | ExprStmt of EExpr.t
  | Assign of Id.t * EType.t option * EExpr.t
  | GAssign of Id.t * EExpr.t
  | FieldAssign of EExpr.t * EExpr.t * EExpr.t
  | FieldDelete of EExpr.t * EExpr.t
  | If of (EExpr.t * t * metadata_t list) list * (t * metadata_t list) option
  | While of EExpr.t * t
  | ForEach of Id.t * EExpr.t * t * metadata_t list * (string * string) option
  | RepeatUntil of t * EExpr.t option * metadata_t list
  | Switch of EExpr.t * (EExpr.t * t) list * t option * string
  | MatchWith of EExpr.t * (EPat.t * t) list
  | Lambda of Id.t * string * Id.t list * Id.t list * t
  | MacroApply of Id.t * EExpr.t list
  | Throw of EExpr.t
  | Fail of EExpr.t
  | Assert of EExpr.t
  | Wrapper of metadata_t list * t

let default () : t = ?@Skip

let rec pp (fmt : Fmt.t) (s : t) : unit =
  let open Fmt in
  let pp_return fmt e =
    if EExpr.isvoid e then () else fprintf fmt " %a" EExpr.pp e
  in
  match s.it with
  | Skip -> fprintf fmt "skip"
  | Debug s' -> fprintf fmt "# %a" pp s'
  | Block ss -> fprintf fmt "{\n%a\n}" (pp_lst ";\n" pp) ss
  | Print e -> fprintf fmt "print %a" EExpr.pp e
  | Return e -> fprintf fmt "return%a" pp_return e
  | ExprStmt e -> EExpr.pp fmt e
  | Assign (x, t, e) ->
    fprintf fmt "%a%a := %a" Id.pp x EType.pp_tannot t EExpr.pp e
  | GAssign (x, e) -> fprintf fmt "|%a| := %a" Id.pp x EExpr.pp e
  | FieldAssign (oe, fe, e) ->
    fprintf fmt "%a[%a] := %a" EExpr.pp oe EExpr.pp fe EExpr.pp e
  | FieldDelete (oe, fe) -> fprintf fmt "delete %a[%a]" EExpr.pp oe EExpr.pp fe
  | If ([], _) -> Eslerr.(internal __FUNCTION__ (Expecting "non-empty if cases"))
  | If (ifcs :: elifcss, elsecs) ->
    let pp_if fmt (e, s, _) = fprintf fmt "if (%a) %a" EExpr.pp e pp s in
    let pp_elif fmt (e, s, _) = fprintf fmt " elif (%a) %a" EExpr.pp e pp s in
    let pp_else fmt (s, _) = fprintf fmt " else %a" pp s in
    fprintf fmt "%a%a%a" pp_if ifcs (pp_lst "" pp_elif) elifcss (pp_opt pp_else)
      elsecs
  | While (e, s') -> fprintf fmt "while (%a) %a" EExpr.pp e pp s'
  | ForEach (x, e, s', _, _) ->
    fprintf fmt "foreach (%a : %a) %a" Id.pp x EExpr.pp e pp s'
  | RepeatUntil (s', e, _) ->
    let pp_until fmt e = fprintf fmt " until %a" EExpr.pp e in
    fprintf fmt "repeat %a%a" pp s' (pp_opt pp_until) e
  | Switch (e, css, dflt, _) ->
    let pp_case fmt (e, s) = fprintf fmt "\ncase %a: %a" EExpr.pp e pp s in
    let pp_default fmt s = fprintf fmt "\nsdefault: %a" pp s in
    fprintf fmt "switch (%a) {%a%a\n}" EExpr.pp e (pp_lst "" pp_case) css
      (pp_opt pp_default) dflt
  | MatchWith (e, css) ->
    let pp_case fmt (pat, s) = fprintf fmt "\n| %a -> %a" EPat.pp pat pp s in
    fprintf fmt "match %a with %a" EExpr.pp e (pp_lst "" pp_case) css
  | Lambda (x, _, pxs, ctxvars, s') ->
    fprintf fmt "%a := lambda (%a) [%a] %a" Id.pp x (pp_lst ", " Id.pp) pxs
      (pp_lst ", " Id.pp) ctxvars pp s'
  | MacroApply (m, es) ->
    fprintf fmt "@%a(%a)" Id.pp m (pp_lst ", " EExpr.pp) es
  | Throw e -> fprintf fmt "throw %a" EExpr.pp e
  | Fail e -> fprintf fmt "fail %a" EExpr.pp e
  | Assert e -> fprintf fmt "assert %a" EExpr.pp e
  | Wrapper (_, s) -> fprintf fmt "gen_wrapper %a" pp s

let str (s : t) : string = Fmt.asprintf "%a" pp s

let rec map ?(emapper : EExpr.t -> EExpr.t = EExpr.Mapper.id) (mapper : t -> t)
  (s : t) : t =
  let map' = map ~emapper mapper in
  let mapper' s' = mapper (s' @> s.at) in
  let id_mapper (x : Id.t) =
    match (emapper ?@(EExpr.Var x.it)).it with
    | EExpr.Var y -> y @> x.at
    | _ -> Eslerr.(internal __FUNCTION__ (Expecting "var expression in LHS"))
  in
  mapper'
  @@
  match s.it with
  | Skip -> Skip
  | Debug s' -> Debug (map' s')
  | Block ss -> Block (List.map map' ss)
  | Print e -> Print (emapper e)
  | Return e -> Return (emapper e)
  | ExprStmt e -> ExprStmt (emapper e)
  | Assign (x, t, e) -> Assign (id_mapper x, t, emapper e)
  | GAssign (x, e) -> GAssign (id_mapper x, emapper e)
  | FieldAssign (oe, fe, e) -> FieldAssign (emapper oe, emapper fe, emapper e)
  | FieldDelete (oe, fe) -> FieldDelete (emapper oe, emapper fe)
  | If (ifcs, elsecs) ->
    let map_ifcs (e, s, meta) = (emapper e, map' s, meta) in
    let map_elsecs (s, meta) = (map' s, meta) in
    If (List.map map_ifcs ifcs, Option.map map_elsecs elsecs)
  | While (e, s') -> While (emapper e, map' s')
  | ForEach (x, e, s', meta, var_meta) ->
    ForEach (id_mapper x, emapper e, map' s', meta, var_meta)
  | RepeatUntil (s', e, meta) ->
    RepeatUntil (map' s', Option.map emapper e, meta)
  | Switch (e, css, dflt, meta) ->
    let map_cs (e, s) = (emapper e, map' s) in
    Switch (emapper e, List.map map_cs css, Option.map map' dflt, meta)
  | MatchWith (e, css) ->
    let map_cs (pat, s) = (pat, map' s) in
    MatchWith (emapper e, List.map map_cs css)
  | Lambda (x, id, pxs, ctxvars, s') -> Lambda (x, id, pxs, ctxvars, map' s')
  | MacroApply (m, es) -> MacroApply (m, List.map emapper es)
  | Throw e -> Throw (emapper e)
  | Fail e -> Fail (emapper e)
  | Assert e -> Assert (emapper e)
  | Wrapper (meta, s') -> Wrapper (meta, map' s')

let rec to_list ?(recursion : bool = false) (to_list_f : t -> 'a list) (s : t) :
  'a list =
  let to_list_s = to_list ~recursion to_list_f in
  let to_list_ss stmts = List.concat (List.map to_list_s stmts) in
  let to_list_recursive () =
    match s.it with
    | Skip | Print _ | Return _ | ExprStmt _ | Assign _ | GAssign _
    | FieldAssign _ | FieldDelete _ | MacroApply _ | Throw _ | Fail _ | Assert _
    | Wrapper _ ->
      []
    | Debug s' -> to_list_s s'
    | Block ss -> to_list_ss ss
    | If (ifcss, elsecs) ->
      to_list_ss
        ( List.map (fun (_, s, _) -> s) ifcss
        @ Option.fold ~none:[] ~some:(fun (s, _) -> [ s ]) elsecs )
    | While (_, s') -> to_list_s s'
    | ForEach (_, _, s', _, _) -> to_list_s s'
    | RepeatUntil (s', _, _) -> to_list_s s'
    | Switch (_, css, dlft, _) ->
      to_list_ss
        ( List.map (fun (_, s) -> s) css
        @ Option.fold ~none:[] ~some:(fun s -> [ s ]) dlft )
    | MatchWith (_, css) -> to_list_ss (List.map (fun (_, s) -> s) css)
    | Lambda (_, _, _, _, s) -> to_list_s s
  in
  to_list_f s @ if not recursion then [] else to_list_recursive ()

module Mapper = struct
  let id (s : t) : t = s
end

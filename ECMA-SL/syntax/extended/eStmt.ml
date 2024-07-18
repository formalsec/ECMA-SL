open EslBase
open Source

type t = t' Source.t

and t' =
  | Skip
  | Debug of t
  | Block of t list
  | ExprStmt of EExpr.t
  | Print of EExpr.t
  | Return of EExpr.t
  | Assert of EExpr.t
  | Fail of EExpr.t
  | Throw of EExpr.t
  | MacroApply of Id.t * EExpr.t list
  | Assign of Id.t * EType.t option * EExpr.t
  | GAssign of Id.t * EExpr.t
  | FieldAssign of EExpr.t * EExpr.t * EExpr.t
  | FieldDelete of EExpr.t * EExpr.t
  | Lambda of Id.t * string * Id.t list * Id.t list * t
  | If of EExpr.t * t * t option
  | While of EExpr.t * t
  | ForEach of Id.t * EExpr.t * t
  | RepeatUntil of t * EExpr.t option
  | Switch of EExpr.t * (EExpr.t * t) list * t option
  | MatchWith of EExpr.t * Id.t option * (EPat.t * t) list

let default : unit -> t =
  let dlft = Skip @> none in
  fun () -> dlft

let rec pp (ppf : Fmt.t) (stmt : t) : unit =
  let pp_vs pp_v ppf es = Fmt.(pp_lst !>", " pp_v) ppf es in
  let pp_indent pp_v ppf = Fmt.fmt ppf "@[<v 2>  %a@]" pp_v in
  let pp_cont s = Fmt.dprintf (match s.it with Block _ -> " " | _ -> "@\n") in
  let pp_nested ppf s =
    match s.it with
    | Block _ -> Fmt.fmt ppf " %a" pp s
    | _ -> Fmt.fmt ppf "@\n%a" (pp_indent pp) s
  in
  let pp_else s1 ppf s2 =
    match s2.it with
    | If _ -> Fmt.fmt ppf "%telse %a" (pp_cont s1) pp s2
    | _ -> Fmt.fmt ppf "%telse%a" (pp_cont s1) pp_nested s2
  in
  match stmt.it with
  | Skip -> Fmt.pp_str ppf ";"
  | Debug s -> Fmt.fmt ppf "# %a" pp s
  | Block ss -> Fmt.fmt ppf "{@\n%a@\n}" (pp_indent Fmt.(pp_lst !>"@\n" pp)) ss
  | ExprStmt e -> Fmt.fmt ppf "%a;" EExpr.pp e
  | Print e -> Fmt.fmt ppf "print %a;" EExpr.pp e
  | Return e ->
    if EExpr.isvoid e then Fmt.pp_str ppf "return;"
    else Fmt.fmt ppf "return %a;" EExpr.pp e
  | Assert e -> Fmt.fmt ppf "assert %a;" EExpr.pp e
  | Fail e -> Fmt.fmt ppf "fail %a;" EExpr.pp e
  | Throw e -> Fmt.fmt ppf "throw %a;" EExpr.pp e
  | MacroApply (m, es) -> Fmt.fmt ppf "@%a(%a);" Id.pp m (pp_vs EExpr.pp) es
  | Assign (x, t, e) ->
    Fmt.fmt ppf "%a%a := %a;" Id.pp x EType.tannot_pp t EExpr.pp e
  | GAssign (x, e) -> Fmt.fmt ppf "|%a| := %a;" Id.pp x EExpr.pp e
  | FieldAssign (oe, fe, e) ->
    Fmt.fmt ppf "%a[%a] := %a;" EExpr.pp oe EExpr.pp fe EExpr.pp e
  | FieldDelete (oe, fe) -> Fmt.fmt ppf "delete %a[%a];" EExpr.pp oe EExpr.pp fe
  | If (e, s1, s2) ->
    let pp_else = Fmt.pp_opt (pp_else s1) in
    Fmt.fmt ppf "if (%a)%a%a" EExpr.pp e pp_nested s1 pp_else s2
  | While (e, s) -> Fmt.fmt ppf "while (%a)%a" EExpr.pp e pp_nested s
  | ForEach (x, e, s) ->
    Fmt.fmt ppf "foreach (%a : %a)%a" Id.pp x EExpr.pp e pp_nested s
  | RepeatUntil (s, e) ->
    let pp_until ppf e = Fmt.fmt ppf "%tuntil (%a);" (pp_cont s) EExpr.pp e in
    Fmt.fmt ppf "repeat %a%a" pp_nested s (Fmt.pp_opt pp_until) e
  | Switch (e, css, dflt) ->
    let pp_dflt_cs ppf s = Fmt.fmt ppf "default:%a" pp_nested s in
    let pp_dflt ppf s = (Fmt.pp_opt pp_dflt_cs) ppf s in
    let pp_cs ppf (v, s) = Fmt.fmt ppf "case %a:%a" EExpr.pp v pp_nested s in
    let pp_css ppf css = Fmt.(pp_lst !>"@\n" pp_cs) ppf css in
    let pp ppf (css, dflt) = Fmt.fmt ppf "%a%a" pp_css css pp_dflt dflt in
    Fmt.fmt ppf "switch (%a) {@\n%a@\n}" EExpr.pp e (pp_indent pp) (css, dflt)
  | MatchWith (e, dsc, css) ->
    let pp_dsc_v ppf dsc = Fmt.fmt ppf " : %a" Id.pp dsc in
    let pp_dsc ppf dsc = Fmt.pp_opt pp_dsc_v ppf dsc in
    let pp_cs ppf (pat, s) = Fmt.fmt ppf "| %a ->%a" EPat.pp pat pp_nested s in
    let pp_css ppf css = Fmt.(pp_lst !>"@\n" pp_cs) ppf css in
    Fmt.fmt ppf "match %a%a with@\n%a" EExpr.pp e pp_dsc dsc pp_css css
  | Lambda (x, _, pxs, ctxvars, s) ->
    Fmt.fmt ppf "%a := lambda (%a) [%a] %a;" Id.pp x (pp_vs Id.pp) pxs
      (pp_vs Id.pp) ctxvars pp s

let str (stmt : t) : string = Fmt.str "%a" pp stmt [@@inline]

let rec map ?(emapper : EExpr.t -> EExpr.t = EExpr.Mapper.id) (mapper : t -> t)
  (stmt : t) : t =
  let map' s = map ~emapper mapper s in
  let mapper' s = mapper (s @> stmt.at) in
  let id_mapper (x : Id.t) =
    match (emapper (EExpr.Var x.it @> none)).it with
    | EExpr.Var y -> y @> x.at
    | e -> Log.fail "expecting var in LHS, but got %a" EExpr.pp (e @> none)
  in
  mapper'
  @@
  match stmt.it with
  | Skip -> Skip
  | Debug s' -> Debug (map' s')
  | Block ss -> Block (List.map map' ss)
  | ExprStmt e -> ExprStmt (emapper e)
  | Print e -> Print (emapper e)
  | Return e -> Return (emapper e)
  | Assert e -> Assert (emapper e)
  | Fail e -> Fail (emapper e)
  | Throw e -> Throw (emapper e)
  | MacroApply (m, es) -> MacroApply (m, List.map emapper es)
  | Assign (x, t, e) -> Assign (id_mapper x, t, emapper e)
  | GAssign (x, e) -> GAssign (id_mapper x, emapper e)
  | FieldAssign (oe, fe, e) -> FieldAssign (emapper oe, emapper fe, emapper e)
  | FieldDelete (oe, fe) -> FieldDelete (emapper oe, emapper fe)
  | Lambda (x, id, pxs, ctxvars, s) -> Lambda (x, id, pxs, ctxvars, map' s)
  | If (e, s1, s2) -> If (emapper e, map' s1, Option.map map' s2)
  | While (e, s) -> While (emapper e, map' s)
  | ForEach (x, e, s) -> ForEach (id_mapper x, emapper e, map' s)
  | RepeatUntil (s, e) -> RepeatUntil (map' s, Option.map emapper e)
  | Switch (e, css, dflt) ->
    let map_cs (e, s) = (emapper e, map' s) in
    Switch (emapper e, List.map map_cs css, Option.map map' dflt)
  | MatchWith (e, dsc, css) ->
    let map_cs (pat, s) = (pat, map' s) in
    MatchWith (emapper e, Option.map id_mapper dsc, List.map map_cs css)

let rec to_list ?(recursion : bool = false) (to_list_f : t -> 'a list) (stmt : t)
  : 'a list =
  let to_list_s s = to_list ~recursion to_list_f s in
  let to_list_ss stmts = List.concat (List.map to_list_s stmts) in
  let to_list_opt s = Option.fold ~none:[] ~some:(fun s -> [ s ]) s in
  let to_list_recursive () =
    match stmt.it with
    | Skip -> []
    | Debug s -> to_list_s s
    | Block ss -> to_list_ss ss
    | ExprStmt _ -> []
    | Print _ | Return _ -> []
    | Assert _ | Fail _ | Throw _ -> []
    | MacroApply _ -> []
    | Assign _ | GAssign _ -> []
    | FieldAssign _ | FieldDelete _ -> []
    | If (_, s1, s2) -> to_list_ss (s1 :: to_list_opt s2)
    | While (_, s) -> to_list_s s
    | ForEach (_, _, s) -> to_list_s s
    | RepeatUntil (s, _) -> to_list_s s
    | Switch (_, css, dflt) -> to_list_ss (List.map snd css @ to_list_opt dflt)
    | MatchWith (_, _, css) -> to_list_ss (List.map snd css)
    | Lambda (_, _, _, _, s) -> to_list_s s
  in
  to_list_f stmt @ if not recursion then [] else to_list_recursive ()

module Mapper = struct
  let id : t -> t = Fun.id
end

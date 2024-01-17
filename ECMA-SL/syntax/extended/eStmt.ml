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
  | Return of EExpr.t option
  | ExprStmt of EExpr.t
  | Assign of string * EType.t option * EExpr.t
  | GlobAssign of string * EExpr.t
  | FieldAssign of EExpr.t * EExpr.t * EExpr.t
  | FieldDelete of EExpr.t * EExpr.t
  | If of EExpr.t * t * t option * metadata_t list * metadata_t list
  | EIf of (EExpr.t * t * metadata_t list) list * (t * metadata_t list) option
  | While of EExpr.t * t
  | ForEach of string * EExpr.t * t * metadata_t list * (string * string) option
  | RepeatUntil of t * EExpr.t * metadata_t list
  | Switch of EExpr.t * (EExpr.t * t) list * t option * string
  | MatchWith of EExpr.t * (EPat.t * t) list
  | Lambda of string * string * string list * string list * t
  | MacroApply of string * EExpr.t list
  | Throw of EExpr.t
  | Fail of EExpr.t
  | Assert of EExpr.t
  | Wrapper of metadata_t list * t

let default () : t = Skip @> no_region

let rec pp (fmt : Fmt.t) (s : t) : unit =
  let open Fmt in
  match s.it with
  | Skip -> ()
  | Debug s' -> fprintf fmt "# %a" pp s'
  | Block ss -> fprintf fmt "{\n%a\n}" (pp_lst ";\n" pp) ss
  | Print e -> fprintf fmt "print %a" EExpr.pp e
  | Return e -> fprintf fmt "return %a" (pp_opt EExpr.pp) e
  | ExprStmt e -> EExpr.pp fmt e
  | Assign (x, t, e) -> fprintf fmt "%s%a := %a" x EType.pp_tannot t EExpr.pp e
  | GlobAssign (x, e) -> fprintf fmt "|%s| := %a" x EExpr.pp e
  | FieldAssign (oe, emapper, e) ->
    fprintf fmt "%a[%a] := %a" EExpr.pp oe EExpr.pp emapper EExpr.pp e
  | FieldDelete (oe, emapper) ->
    fprintf fmt "delete %a[%a]" EExpr.pp oe EExpr.pp emapper
  | If (e, s1, s2, _, _) ->
    let pp_else fmt s2 = fprintf fmt " else %a" pp s2 in
    fprintf fmt "if (%a) %a%a" EExpr.pp e pp s1 (pp_opt pp_else) s2
  | EIf ([], _) ->
    Eslerr.(internal __FUNCTION__ (Expecting "non-empty if cases"))
  | EIf (ifcs :: elifcss, elsecs) ->
    let pp_if fmt (e, s, _) = fprintf fmt "if (%a) %a" EExpr.pp e pp s in
    let pp_elif fmt (e, s, _) = fprintf fmt " elif (%a) %a" EExpr.pp e pp s in
    let pp_else fmt (s, _) = fprintf fmt " else %a" pp s in
    fprintf fmt "%a%a%a" pp_if ifcs (pp_lst "" pp_elif) elifcss (pp_opt pp_else)
      elsecs
  | While (e, s') -> fprintf fmt "while (%a) %a" EExpr.pp e pp s'
  | ForEach (x, e, s', _, _) ->
    fprintf fmt "foreach (%s : %a) %a" x EExpr.pp e pp s'
  | RepeatUntil (s', e, _) -> fprintf fmt "repeat %a until %a" pp s' EExpr.pp e
  | Switch (e, css, dflt, _) ->
    let pp_case fmt (e, s) = fprintf fmt "\ncase %a: %a" EExpr.pp e pp s in
    let pp_default fmt s = fprintf fmt "\nsdefault: %a" pp s in
    fprintf fmt "switch (%a) {%a%a\n}" EExpr.pp e (pp_lst "" pp_case) css
      (pp_opt pp_default) dflt
  | MatchWith (e, css) ->
    let pp_case fmt (pat, s) = fprintf fmt "\n| %a -> %a" EPat.pp pat pp s in
    fprintf fmt "match %a with %a" EExpr.pp e (pp_lst "" pp_case) css
  | Lambda (x, _, params, ctxvars, s') ->
    fprintf fmt "%s := lambda (%a) [%a] %a" x (pp_lst ", " pp_str) params
      (pp_lst ", " pp_str) ctxvars pp s'
  | MacroApply (m, es) -> fprintf fmt "@%s (%a)" m (pp_lst ", " EExpr.pp) es
  | Throw e -> fprintf fmt "throw %a" EExpr.pp e
  | Fail e -> fprintf fmt "fail %a" EExpr.pp e
  | Assert e -> fprintf fmt "assert %a" EExpr.pp e
  | Wrapper (_, s) -> fprintf fmt "gen_wrapper %a" pp s

let str (s : t) : string = Fmt.asprintf "%a" pp s

let rec map ?(emapper : EExpr.t -> EExpr.t = fun e -> e) (mapper : t -> t)
  (s : t) : t =
  let map' = map ~emapper mapper in
  let mapper' s' = mapper (s' @> s.at) in
  let var_mapper x =
    match emapper (EExpr.Var x) with
    | EExpr.Var y -> y
    | _ -> Eslerr.(internal __FUNCTION__ (Expecting "var expression in LHS"))
  in
  mapper'
  @@
  match s.it with
  | Skip -> Skip
  | Debug s' -> Debug (map' s')
  | Block ss -> Block (List.map map' ss)
  | Print e -> Print (emapper e)
  | Return e -> Return (Option.map emapper e)
  | ExprStmt e -> ExprStmt (emapper e)
  | Assign (x, t, e) -> Assign (var_mapper x, t, emapper e)
  | GlobAssign (x, e) -> GlobAssign (var_mapper x, emapper e)
  | FieldAssign (oe, fe, e) -> FieldAssign (emapper oe, emapper fe, emapper e)
  | FieldDelete (oe, fe) -> FieldDelete (emapper oe, emapper fe)
  | If (e, s1, s2, meta1, meta2) ->
    If (emapper e, map' s1, Option.map map' s2, meta1, meta2)
  | EIf (ifcs, elsecs) ->
    let map_ifcs (e, s, meta) = (emapper e, map' s, meta) in
    let map_elsecs (s, meta) = (map' s, meta) in
    EIf (List.map map_ifcs ifcs, Option.map map_elsecs elsecs)
  | While (e, s') -> While (emapper e, map' s')
  | ForEach (x, e, s', meta, var_meta) ->
    ForEach (var_mapper x, emapper e, map' s', meta, var_meta)
  | RepeatUntil (s', e, meta) -> RepeatUntil (map' s', emapper e, meta)
  | Switch (e, css, dflt, meta) ->
    let map_cs (e, s) = (emapper e, map' s) in
    Switch (emapper e, List.map map_cs css, Option.map map' dflt, meta)
  | MatchWith (e, css) ->
    let map_cs (pat, s) = (pat, map' s) in
    MatchWith (emapper e, List.map map_cs css)
  | Lambda (x, id, params, ctxvars, s') ->
    Lambda (x, id, params, ctxvars, map' s')
  | MacroApply (m, es) -> MacroApply (m, List.map emapper es)
  | Throw e -> Throw (emapper e)
  | Fail e -> Fail (emapper e)
  | Assert e -> Assert (emapper e)
  | Wrapper (meta, s') -> Wrapper (meta, map' s')

(* FIXME: Requires cleaning *)
let subst (sbst : EExpr.subst_t) (s : t) : t =
  (*Printf.printf "Applying the subst: %s\nOn statement:\n%s\n" (EExpr.string_of_subst sbst) (str s); *)
  let ret = map ~emapper:(EExpr.subst sbst) (fun x -> x) s in
  (* Printf.printf "Obtained: %s\n" (str ret);  *)
  ret

let rec to_list (is_rec : t -> bool) (f : t -> 'a list) (s : t) : 'a list =
  let f' = to_list is_rec f in
  let f_stmts stmts = List.concat (List.map f' stmts) in
  let f_o so = Option.fold ~some:f' ~none:[] so in
  let f_pat pats = List.concat (List.map (fun (_, s) -> f' s) pats) in
  let f_cases cases = List.map (fun (_, s) -> s) cases in
  let f_if_elses if_elses = List.map (fun (_, s, _) -> s) if_elses in
  let ret = f s in
  if not (is_rec s) then ret
  else
    let ret_rec =
      match s.it with
      | Skip | Print _ | Wrapper _ | Assign _ | GlobAssign _ | Return _
      | FieldAssign _ | FieldDelete _ | ExprStmt _ | Throw _ | Fail _ | Assert _
        ->
        []
      | Debug s' -> f' s'
      | Block stmts -> f_stmts stmts
      | If (_e, st, sf, _, _) -> f' st @ f_o sf
      | EIf (ifs, final_else) ->
        f_stmts
          ( f_if_elses ifs
          @ Option.fold ~some:(fun (s, _) -> [ s ]) ~none:[] final_else )
      | While (_e, s) -> f' s
      | ForEach (_x, _e, s, _, _) -> f' s
      | RepeatUntil (s, _e, _) -> f' s
      | MatchWith (_e, pats) -> f_pat pats
      | Lambda (_, _, _, _, s) -> f' s
      | MacroApply _ -> failwith "EStmt.to_list on MacroApply"
      | Switch (_, cases, so, _) ->
        f_stmts (f_cases cases @ Option.fold ~some:(fun x -> [ x ]) ~none:[] so)
    in
    ret @ ret_rec

let lambdas (s : t) : (string * string list * string list * t) list =
  let f_l s =
    match s.it with
    | Lambda (_, fid, xs, ys, s) -> [ (fid, ys, xs, s) ]
    | _ -> []
  in
  let f_rec _s = true in
  to_list f_rec f_l s

let return_val (expr_opt : EExpr.t option) : EExpr.t =
  Option.value ~default:(EExpr.Val Val.Null) expr_opt

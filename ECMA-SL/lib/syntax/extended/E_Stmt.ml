open Source

type metadata_t = { where : string; html : string }
type stmt = Assume of E_Expr.t

type t = t' Source.phrase

and t' =
  | Skip
  | Fail of E_Expr.t
  | Throw of E_Expr.t
  | Print of E_Expr.t
  | Return of E_Expr.t option
  | Wrapper of metadata_t list * t
  | Assign of string * E_Type.t option * E_Expr.t
  | GlobAssign of string * E_Expr.t
  | Block of t list
  | If of E_Expr.t * t * t option * metadata_t list * metadata_t list
      (** "If" and "Else" metadata *)
  | EIf of (E_Expr.t * t * metadata_t list) list * (t * metadata_t list) option
  | While of E_Expr.t * t
  | ForEach of
      string * E_Expr.t * t * metadata_t list * (string * string) option
  | FieldAssign of E_Expr.t * E_Expr.t * E_Expr.t
  | FieldDelete of E_Expr.t * E_Expr.t
  | ExprStmt of E_Expr.t
  | RepeatUntil of t * E_Expr.t * metadata_t list
  | MatchWith of E_Expr.t * (E_Pat.t * t) list
  | MacroApply of string * E_Expr.t list
  | Switch of E_Expr.t * (E_Expr.t * t) list * t option * string
      (** metadata; just "table caption" for now. *)
  | Lambda of string * string * string list * string list * t
  | Abort of E_Expr.t
  | Assert of E_Expr.t
  | SymStmt of stmt

let is_basic (s : t) : bool =
  match s.it with
  | If _ | While _ | RepeatUntil _ | Block _ -> false
  | _ -> true

let default () : t' = Skip

let rec str (stmt : t) : string =
  let str_cases cases =
    let strs =
      List.map
        (fun (e, s) -> Printf.sprintf "case %s: %s" (E_Expr.str e) (str s))
        cases
    in
    String.concat "\n" strs
  in

  let str_o =
    Option.map_default (fun s -> Printf.sprintf "default: %s" (str s)) ""
  in

  match stmt.it with
  | Skip -> ""
  | Fail e -> "fail " ^ E_Expr.str e
  | Throw e -> "throw " ^ E_Expr.str e
  | Print e -> "print " ^ E_Expr.str e
  | Assert e -> "assert " ^ E_Expr.str e
  | Return None -> "return"
  | Return (Some e) -> "return " ^ E_Expr.str e
  | Wrapper (_m, s) -> str s
  | Assign (x, t, exp) ->
      let x' = match t with None -> x | Some t' -> x ^ ": " ^ E_Type.str t' in
      x' ^ " := " ^ E_Expr.str exp
  | GlobAssign (x, exp) -> "|" ^ x ^ "| := " ^ E_Expr.str exp
  | Block stmts -> "{ " ^ String.concat ";" (List.map str stmts) ^ " }"
  | If (e, s1, s2, _, _) -> (
      let v = "if (" ^ E_Expr.str e ^ ") " ^ str s1 in
      match s2 with None -> v | Some s -> v ^ " else " ^ str s)
  | EIf (ifs, final_else) -> (
      let ifs' =
        List.map
          (fun (e, s, _) -> Printf.sprintf "if (%s) %s" (E_Expr.str e) (str s))
          ifs
      in
      let if_elses = String.concat " else " ifs' in
      match final_else with
      | None -> if_elses
      | Some (s, _) -> Printf.sprintf "%s else %s" if_elses (str s))
  | While (exp, s) -> "while (" ^ E_Expr.str exp ^ ") " ^ str s
  | ForEach (x, exp, s, _, _) ->
      Printf.sprintf "foreach (%s, %s) %s" x (E_Expr.str exp) (str s)
  | FieldAssign (e_o, f, e_v) ->
      E_Expr.str e_o ^ "[" ^ E_Expr.str f ^ "] := " ^ E_Expr.str e_v
  | FieldDelete (e, f) -> "delete " ^ E_Expr.str e ^ "[" ^ E_Expr.str f ^ "]"
  | ExprStmt e -> E_Expr.str e
  | RepeatUntil (s, e, _) -> "repeat " ^ str s ^ " until " ^ E_Expr.str e
  | MatchWith (e, pats_stmts) ->
      "match " ^ E_Expr.str e ^ " with | "
      ^ String.concat " | "
          (List.map (fun (e, s) -> E_Pat.str e ^ ": " ^ str s) pats_stmts)
  | MacroApply (m, es) ->
      "@" ^ m ^ " (" ^ String.concat ", " (List.map E_Expr.str es) ^ ")"
  | Switch (e, cases, so, _) ->
      Printf.sprintf "switch (%s) { %s %s }" (E_Expr.str e) (str_cases cases)
        (str_o so)
  | Lambda (x, fid, xs, ys, s) ->
      Printf.sprintf "%s := lambda <%s> (%s; %s) { %s }" x fid
        (String.concat ", " xs) (String.concat ", " ys) (str s)
  | Abort e -> "se_abort " ^ E_Expr.str e
  | SymStmt (Assume e) -> "se_assume " ^ E_Expr.str e

let return_val (expr_opt : E_Expr.t option) : E_Expr.t =
  Option.default (E_Expr.Val Val.Null) expr_opt

let rec map ?(fe : (E_Expr.t -> E_Expr.t) option) (f : t -> t) (s : t) : t =
  let fe = Option.default (fun x -> x) fe in
  let f_pat = List.map (fun (epat, s) -> (epat, map ~fe f s)) in
  let f_cases = List.map (fun (e, s) -> (fe e, map ~fe f s)) in
  let f_if_elses = List.map (fun (e, s, m) -> (fe e, map ~fe f s, m)) in

  let fx (x : string) : string =
    let e' = fe (E_Expr.Var x) in
    match (e' : E_Expr.t) with
    | E_Expr.Var y -> y
    | _ -> raise (Failure "Substituting non-var expression on LHS")
  in

  let s' =
    match s.it with
    | Skip -> Skip
    | Fail e -> Fail (fe e)
    | Throw e -> Throw (fe e)
    | Print e -> Print (fe e)
    | Assert e -> Assert (fe e)
    | Return None -> Return None
    | Return (Some e) -> Return (Some (fe e))
    | Wrapper (m, s) -> Wrapper (m, map ~fe f s)
    | Assign (x, t, e) -> Assign (fx x, t, fe e)
    | GlobAssign (x, e) -> GlobAssign (fx x, fe e)
    | Block ss -> Block (List.map (map ~fe f) ss)
    | If (e, s1, s2, m_i, m_e) ->
        If (fe e, map ~fe f s1, Option.map (map ~fe f) s2, m_i, m_e)
    | EIf (ifs, final_else) ->
        EIf
          ( f_if_elses ifs,
            Option.map (fun (s, m) -> (map ~fe f s, m)) final_else )
    | While (e, s) -> While (fe e, map ~fe f s)
    | ForEach (x, e, s, m, v_m) -> ForEach (fx x, fe e, map ~fe f s, m, v_m)
    | FieldAssign (e_o, e_f, e_v) -> FieldAssign (fe e_o, fe e_f, fe e_v)
    | FieldDelete (e, f) -> FieldDelete (fe e, fe f)
    | ExprStmt e -> ExprStmt (fe e)
    | RepeatUntil (s, e, m) -> RepeatUntil (map ~fe f s, fe e, m)
    | MatchWith (e, pats_stmts) -> MatchWith (fe e, f_pat pats_stmts)
    | MacroApply (m, es) -> MacroApply (m, List.map fe es)
    | Switch (e, cases, so, meta) ->
        Switch (fe e, f_cases cases, Option.map (map ~fe f) so, meta)
    | Lambda (z, id, xs, ys, s) -> Lambda (z, id, xs, ys, map ~fe f s)
    | Abort e -> Abort (fe e)
    | SymStmt (Assume e) -> SymStmt (Assume (fe e))
  in
  f (s' @> s.at)

let subst (sbst : E_Expr.subst_t) (s : t) : t =
  (*Printf.printf "Applying the subst: %s\nOn statement:\n%s\n" (E_Expr.string_of_subst sbst) (str s); *)
  let ret = map ~fe:(E_Expr.subst sbst) (fun x -> x) s in
  (* Printf.printf "Obtained: %s\n" (str ret);  *)
  ret

let rec to_list (is_rec : t -> bool) (f : t -> 'a list) (s : t) : 'a list =
  let f' = to_list is_rec f in
  let f_stmts stmts = List.concat (List.map f' stmts) in
  let f_o so = Option.map_default f' [] so in
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
      | Abort _
      | SymStmt (Assume _) ->
          []
      | Block stmts -> f_stmts stmts
      | If (_e, st, sf, _, _) -> f' st @ f_o sf
      | EIf (ifs, final_else) ->
          f_stmts
            (f_if_elses ifs
            @ Option.map_default (fun (s, _) -> [ s ]) [] final_else)
      | While (_e, s) -> f' s
      | ForEach (_x, _e, s, _, _) -> f' s
      | RepeatUntil (s, _e, _) -> f' s
      | MatchWith (_e, pats) -> f_pat pats
      | Lambda (_, _, _, _, s) -> f' s
      | MacroApply _ -> failwith "E_Stmt.to_list on MacroApply"
      | Switch (_, cases, so, _) ->
          f_stmts (f_cases cases @ Option.map_default (fun x -> [ x ]) [] so)
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

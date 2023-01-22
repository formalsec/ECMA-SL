open HTMLExtensions
open HTMLExtensions.E_Stmt

let compare_funcs (f1 : E_Func.t) (f2 : E_Func.t) : int =
  let f1_metadata = E_Func.get_metadata f1 in
  let f2_metadata = E_Func.get_metadata f2 in
  match (f1_metadata, f2_metadata) with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some m1, Some m2 -> E_Func_Metadata.compare_sec_names m1 m2

let filter_funcs (f : E_Func.t) : bool =
  match E_Func.get_metadata f with None -> false | Some _ -> true

type std_t =
  | Leaf of E_Func.t * E_Func.t list
  | Node of E_Func.t option * std_t list

let prefixes_match (level : int) (f1 : E_Func.t) (f2 : E_Func.t) : bool =
  let meta1 = E_Func.get_metadata f1 in
  let meta2 = E_Func.get_metadata f2 in
  match (meta1, meta2) with
  | Some m1, Some m2 ->
      let sec_number1 = E_Func_Metadata.get_section_number m1 in
      let sec_number2 = E_Func_Metadata.get_section_number m2 in
      let sec_number1_split = String.split_on_char '.' sec_number1 in
      let sec_number2_split = String.split_on_char '.' sec_number2 in
      if
        List.length sec_number1_split < level
        || List.length sec_number2_split < level
      then false
      else
        let arr1 = Array.of_list sec_number1_split in
        let arr2 = Array.of_list sec_number2_split in
        let sub_arr1 = Array.sub arr1 0 level in
        let sub_arr2 = Array.sub arr2 0 level in
        let lst1 = Array.to_list sub_arr1 in
        let lst2 = Array.to_list sub_arr2 in
        let lst_c = List.combine lst1 lst2 in
        List.for_all (fun (s1, s2) -> s1 = s2) lst_c
  | _ -> false

let group_by_level (level : int) (funcs : (E_Func.t * E_Func.t list) list) :
    (E_Func.t * E_Func.t list) list list =
  List.rev
    (List.fold_left
       (fun acc (f, fs) ->
         match acc with
         | [] | [] :: _ -> [ [ (f, fs) ] ]
         | ((f', fs') :: fss) :: fsss when prefixes_match level f f' ->
             (((f', fs') :: fss) @ [ (f, fs) ]) :: fsss
         | ((f', fs') :: fss) :: fsss ->
             [ (f, fs) ] :: ((f', fs') :: fss) :: fsss)
       [] funcs)

let is_subsec (level : int) (f1 : E_Func.t) (f2 : E_Func.t) : bool =
  let meta1 = E_Func.get_metadata f1 in
  let meta2 = E_Func.get_metadata f2 in
  match (meta1, meta2) with
  | Some m1, Some m2 ->
      let sec_number1 = E_Func_Metadata.get_section_number m1 in
      let sec_number2 = E_Func_Metadata.get_section_number m2 in
      let sec_number1_split = String.split_on_char '.' sec_number1 in
      let sec_number2_split = String.split_on_char '.' sec_number2 in
      List.length sec_number1_split < List.length sec_number2_split
      && List.length sec_number1_split < level
  | _ -> false

let group_head (level : int) (group : (E_Func.t * E_Func.t list) list) :
    E_Func.t option * (E_Func.t * E_Func.t list) list =
  match group with
  | (f1, []) :: (f2, fs2) :: fss when is_subsec level f1 f2 ->
      (Some f1, (f2, fs2) :: fss)
  | _ -> (None, group)

let rec create_std (level : int)
    (funcs : E_Func.t option * (E_Func.t * E_Func.t list) list) : std_t =
  match funcs with
  | _, [] -> raise (Failure "ERROR: create_std")
  | None, [ (f, fs) ] -> Leaf (f, fs)
  | Some f, [ (f', fs') ] -> Node (Some f, [ Leaf (f', fs') ])
  | f_head, funcs ->
      let groups = group_by_level level funcs in
      let groups' = List.map (group_head (level + 1)) groups in
      let stds = List.map (create_std (level + 1)) groups' in
      Node (f_head, stds)

let create_std_aux (funcs : E_Func.t list) : (E_Func.t * E_Func.t list) list =
  let f = List.hd funcs in
  List.rev
    (List.fold_left
       (fun (acc : (E_Func.t * E_Func.t list) list) (f' : E_Func.t) :
            (E_Func.t * E_Func.t list) list ->
         let f, fs, fss =
           match acc with
           | (f, fs) :: fss -> (f, fs, fss)
           | _ ->
               invalid_arg
                 "Error in \"HTMLGenerator.create_std_aux\": \"acc\" is \
                  invalid."
         in
         match E_Func.get_metadata f' with
         | None ->
             invalid_arg
               (Printf.sprintf
                  "Error in \"HTMLGenerator.create_std_aux\": function \"%s\" \
                   has no metadata."
                  (E_Func.get_name f'))
         | Some m -> (
             match E_Func_Metadata.get_section_name m with
             | None -> (f, fs @ [ f' ]) :: fss
             | Some _ -> (f', []) :: (f, fs) :: fss))
       [ (f, []) ]
       (List.tl funcs))

let create_std_wrapper (funcs : E_Func.t list) : std_t =
  let funcs' = create_std_aux funcs in
  create_std 1 (None, funcs')

let rec generate_html (std : std_t) : string =
  match std with
  | Leaf (f, fs) ->
      let fs_html =
        String.concat "" (List.map (E_Func.to_html ~inner_sections:"") fs)
      in
      E_Func.to_html ~inner_sections:fs_html f
  | Node (Some f, stds) ->
      let inner_secs = String.concat "" (List.map generate_html stds) in
      E_Func.to_html ~inner_sections:inner_secs f
  | Node (None, stds) ->
      Printf.sprintf "<section>%s</section>"
        (String.concat "" (List.map generate_html stds))

let mapper (new_funcs : (string, E_Func.t) Hashtbl.t) (s : E_Stmt.t) : E_Stmt.t
    =
  match s with
  | MatchWith (e, pats_stmts) ->
      (* Filter MatchWith statements that have metadata *)
      let pats_stmts_filtered =
        List.filter
          (fun ((pat : E_Pat.t), (stmt : E_Stmt.t)) ->
            match pat with E_Pat.ObjPat (_, Some meta) -> true | _ -> false)
          pats_stmts
      in
      (* Convert MatchWith in a E_Func *)
      List.iter
        (fun ((pat : E_Pat.t), (stmt : E_Stmt.t)) ->
          match pat with
          | E_Pat.ObjPat (_, Some meta) ->
              let prod_name =
                match E_Pat_Metadata.get_production_name meta with
                | None -> Val.Null
                | Some s -> Val.Str s
              in
              let prod_to_func_pre =
                match
                  ( E_Pat_Metadata.get_pre meta,
                    E_Pat_Metadata.get_production_text meta )
                with
                | "", "" -> ""
                | "", prod_text ->
                    Printf.sprintf
                      "The production <span class=\"prod\">%s</span> is \
                       evaluated as follows:"
                      prod_text
                | pre, "" -> pre
                | pre, prod_text ->
                    pre ^ "</p><p>"
                    ^ Printf.sprintf
                        "The production <span class=\"prod\">%s</span> is \
                         evaluated as follows:"
                        prod_text
              in
              let func_meta =
                E_Func_Metadata.build_func_metadata
                  [
                    Val.Str (E_Pat_Metadata.get_production_number meta);
                    Val.Str prod_to_func_pre;
                    Val.Str (E_Pat_Metadata.get_post meta);
                    prod_name;
                  ]
                  (E_Pat_Metadata.get_meta_params meta)
              in
              let func_name = "dummy" in
              let newfunc = E_Func.create (Some func_meta) func_name [] stmt in
              Hashtbl.replace new_funcs
                (E_Pat_Metadata.get_production_number meta)
                newfunc
          | _ -> invalid_arg "Unexpected E_Pat")
        pats_stmts_filtered;
      (* Replace MatchWith by a Skip statement *)
      Skip
  | _ -> s

let parse_html_rules (file_name : string) : unit =
  if file_name <> "" then
    let json = HTML_Rules.load_json_file file_name in
    let rules = HTML_Rules.parse_json json in
    HTML_Rules.update_tables rules
(* List.iter (
   fun (f : HTML_Rules.func_call_t) ->
    Hashtbl.add HTML_Rules.func_call_rules f.func_name f
   ) funcs *)

let generate (json_file : string) (prog : E_Prog.t) : string =
  let funcs_list = E_Prog.get_funcs prog in
  (* filter out functions without metadata *)
  let filtered_funcs_list = List.filter filter_funcs funcs_list in
  (* Convert MatchWith to E_Func *)
  let converted_table = Hashtbl.create 0 in
  let converted_funcs_list =
    List.map
      (fun f ->
        E_Func.create (E_Func.get_metadata f) (E_Func.get_name f)
          (E_Func.get_params f)
          (E_Stmt.map (mapper converted_table) (E_Func.get_body f)))
      filtered_funcs_list
  in
  let merged_funcs =
    List.of_seq (Hashtbl.to_seq_values converted_table) @ converted_funcs_list
  in
  (* sort funcs by section number *)
  let sorted_funcs_list = List.fast_sort compare_funcs merged_funcs in
  (* Create std record *)
  let std = create_std_wrapper sorted_funcs_list in
  (* Load function calls and matching phrases from JSON file *)
  parse_html_rules json_file;
  (* Transform to HTML *)
  let body =
    match std with
    (* Just to guarantee that all the chapters/main sections are direct children of the <body> html element
       and not children of another <section> html element *)
    | Node (None, stds) -> String.concat "" (List.map generate_html stds)
    | _ -> generate_html std
  in
  Printf.sprintf
    "<!DOCTYPE html><html><head><link rel=\"stylesheet\" \
     href=\"es5.1.css\"><meta charset=\"utf-8\"></head><body>%s</body></html>"
    body

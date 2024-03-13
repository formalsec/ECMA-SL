exception Except of string

module SSet = Set.Make (String)

module M = struct
  type t = SSet.t ref
  type flow = t * t

  let top : t option ref = ref None
  let flows : flow list ref = ref []
  let all_levels : t list ref = ref []

  let setTop (l_top : t) : unit =
    top := Some l_top;
    ()

  let print_set (set : t) : unit =
    SSet.iter (fun str -> Printf.printf "# %s\n" str) !set;
    ()

  let str_sset (l : SSet.t) : string =
    "{" ^ String.concat " " (SSet.elements l) ^ "}"

  let find_ref (lev : SSet.t) : t =
    let chng = ref true in
    let ref_res = ref lev in
    let ele =
      List.fold_left
        (fun ac ele ->
          if !ele = lev then (
            chng := false;
            (*Printf.printf "Found ref >>>>>%d   (%s)\n" (Obj.magic (ele)) (str_sset !ref_res);*)
            ele )
          else ac )
        (ref SSet.empty) !all_levels
    in
    if !chng then (
      all_levels := !all_levels @ [ ref_res ];
      (*Printf.printf "New ref >>>>>%d   (%s)\n" (Obj.magic (ref_res)) (str_sset !ref_res);*)
      ref_res )
    else ele

  (* Gives an existent ref or a new ref *)

  let get_low () : t = find_ref SSet.empty

  let get_high () : t =
    match !top with
    | None -> raise (Except "SecLev_Dep was not initialized with top element.")
    | Some v -> v

  let str (l : t) : string = "{" ^ String.concat " " (SSet.elements !l) ^ "}"

  let flow_to_str (fl : flow) : string =
    match fl with
    | (fromset, toset) -> Printf.sprintf "%s -> %s" (str fromset) (str toset)

  let apply_flow (fl : flow) (lev : SSet.t) : SSet.t =
    Printf.printf "\tApplying flow %s to lev %s\n" (flow_to_str fl)
      (str_sset lev);
    match fl with
    | (fromset, toset) ->
      if SSet.subset !toset lev then SSet.union !fromset lev else lev

  let close_level (lev : SSet.t) : SSet.t =
    Printf.printf "Closing level %s\n" (str_sset lev);
    let rec loop (_chng : bool) (level : SSet.t) : SSet.t =
      let old_lev = level in
      let lev2 =
        List.fold_left (fun ac flow -> apply_flow flow ac) level !flows
      in
      let chng = not (SSet.equal level old_lev) in
      if chng then loop chng lev2 else lev2
    in
    let res = loop true lev in
    print_string ("\tResulting level: " ^ str_sset res ^ "\n");

    res

  let lub (set1 : t) (set2 : t) : t = find_ref (SSet.union !set1 !set2)
  let lubn (lst : t list) : t = List.fold_left lub (ref SSet.empty) lst
  let leq (set1 : t) (set2 : t) : bool = SSet.subset !set1 !set2

  let update_levels () : unit =
    List.iter (fun refer -> refer := close_level !refer) !all_levels;
    ()

  let addFlow (set1 : t) (set2 : t) : unit =
    flows := !flows @ [ (set1, set2) ];
    print_string "Updating Levels list...\n";
    update_levels ();

    ()

  let parse_lvl (str : string) : t =
    let rem1 = String.split_on_char '{' str in
    let rem2 = String.split_on_char '}' (List.nth rem1 1) in
    let finalst = String.split_on_char ',' (List.nth rem2 0) in
    (*Find de correct re*)
    if finalst = [ "" ] then (
      print_string "IF\n";
      let close = close_level (SSet.of_list []) in
      let refer = find_ref close in
      (*Printf.printf ">>>>>>>>>>>>%d\n" (Obj.magic refer);*)
      refer )
    else (
      print_string "ELSE\n";
      let close = close_level (SSet.of_list finalst) in
      let refer = find_ref close in
      (*Printf.printf ">>>>>>>>>>>>%d\n" (Obj.magic refer);*)
      refer )
end

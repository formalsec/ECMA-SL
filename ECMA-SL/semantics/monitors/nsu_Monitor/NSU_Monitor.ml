(*Each monitor is independent of the other ones*)
open SecCallStack
open SecLabel

module M (SL : SecurityLevel.M) = struct 
(*module SSet = Set.Make(String)*)

exception Except of string

type sl = SL.t

type state_t =  (sl SecCallStack.t) * (sl SecHeap.t) * (sl SecStore.t) * sl list

type monitor_return = | MReturn of state_t
                      | MFail of (state_t * string)


let print_pc (pc : sl list) =
  print_string "[ M - STACK ]";
  let aux= List.rev pc in
  print_string ((String.concat ":: " (List.map SL.str aux))^"\n")

let add_pc (pc : sl list) (lvl : sl) : sl list=
  let aux= List.rev pc in
  let pc'=  [lvl] @ aux in
  List.rev pc'

let pop_pc (pc : sl list) : sl list =
  let pc'= List.rev pc in
  match pc' with
  |[] -> raise(Except "PC list is empty!")
  |l::ls'-> List.rev ls'

let check_pc (pc : sl list) : sl =
  let pc'= List.rev pc in
  match pc' with
  | s::ss'-> s
  | _ -> raise(Except "PC list is empty!")

let expr_lvl (ssto: sl SecStore.t) (exp:Expr.t) : sl =
  (*Criar lub entre lista de variaveis*)
  let vars = Expr.vars exp in
  List.fold_left  (SL.lub) (SL.get_low ())  (List.map (SecStore.get ssto) vars)


let rec eval_small_step (m_state: state_t) (tl:sl SecLabel.t) : monitor_return =

  let (scs, sheap, ssto, pc)= m_state in
  print_string ("Monitor Evaluating >> " ^ (SecLabel.str SL.str tl) ^ "\n");
  print_string "=== MONITOR STATE ===\n";
  print_string ("PC level: "^(SL.str (check_pc pc))^"\n");

             (*
            No-Sensitive-Upgrade
            *)

  match tl with
  | EmptyLab ->
    MReturn (scs,sheap,ssto,pc)

  | MergeLab ->
    let pc' = pop_pc pc in
    MReturn (scs,sheap, ssto,pc')

  | ReturnLab e ->
    let lvl = expr_lvl ssto e in
    let lvl_f = SL.lub lvl (check_pc pc) in
    let (frame, scs') = SecCallStack.pop scs in
    (match frame with
     | Intermediate (pc',ssto', x) ->  eval_small_step (scs', sheap, ssto', pc') (SecLabel.UpgVarLab (x,lvl_f))
     | Toplevel -> MReturn (scs', sheap, ssto, pc))


  | AssignLab (var, exp)->
    let lvl = expr_lvl ssto exp in
    print_string ("lev(" ^(Expr.str exp) ^ ") = " ^ (SL.str lvl)^ "\n");
    let pc_lvl= check_pc pc in
    let var_lvl = SecStore.get_safe ssto var in
    (match var_lvl with
    |Some var_lvl -> 
      print_string ("lev(" ^(var) ^ ") = " ^ (SL.str var_lvl)^ "\n");
      if (SL.leq pc_lvl var_lvl ) then(
        SecStore.set ssto var (SL.lub lvl  pc_lvl);
        print_string ("SECSTORE = "^ var ^" <-"^ (SL.str (SL.lub lvl pc_lvl)) ^ "\n");
        MReturn (scs, sheap, ssto, pc))
      else MFail((scs,sheap,ssto,pc), "Illegal Assignment")
    | None ->
        print_string ("lev(" ^(var) ^ ") = EMPTY\n");  
        SecStore.set ssto var (SL.lub lvl  pc_lvl);
        print_string ("SECSTORE = "^ var ^" <-"^ (SL.str (SL.lub lvl pc_lvl)) ^ "\n");
        MReturn (scs, sheap, ssto, pc))
     

  | PrintLab (exp) ->
    let lvl_pc= check_pc pc in
    let lvl_exp = expr_lvl ssto exp in
    if (SL.leq lvl_pc lvl_exp) then
    MReturn (scs, sheap, ssto, pc)
    else
    MFail ((scs, sheap, ssto, pc), "Illegal Print")
  
  | AssignInObjCheckLab (x, f, o, e_f, e_o) ->
    let lvl_pc= check_pc pc in
    let ef_lvl= expr_lvl ssto e_f in
    let eo_lvl= expr_lvl ssto e_o in
    let ctx_lvl = SL.lubn ([lvl_pc ; ef_lvl ; eo_lvl]) in
    let x_lvl = SecStore.get_safe ssto x in 
    let struct_lvl = SecHeap.get_struct_lvl sheap o in
    (match SecHeap.get_field sheap o f, x_lvl, struct_lvl with
      | Some (lvl, _ ), Some x_lvl, _ ->
          if (SL.leq ctx_lvl x_lvl) then (
            SecStore.set ssto x (SL.lub lvl ctx_lvl);
            MReturn (scs, sheap, ssto, pc))
          else MFail((scs,sheap,ssto,pc), "Illegal Assignment")
      | Some (lvl, _ ), None, _ ->  
          SecStore.set ssto x (SL.lub lvl ctx_lvl);
          MReturn (scs, sheap, ssto, pc)
      | None, Some x_lvl, Some struct_lvl -> 
          if (SL.leq ctx_lvl x_lvl) then (
            SecStore.set ssto x (SL.lub ctx_lvl struct_lvl);
            MReturn (scs, sheap, ssto, pc))
          else MFail((scs,sheap,ssto,pc), "Illegal Assignment")
      | None, None, Some struct_lvl ->
          SecStore.set ssto x (SL.lub ctx_lvl struct_lvl);
          MReturn (scs, sheap, ssto, pc)
      | _ -> raise (Except "Internal Error"))

  | BranchLab (exp,st) ->
    let lev= expr_lvl ssto exp in
    let pc_lvl = check_pc pc in
    let pc' = add_pc pc (SL.lub lev pc_lvl) in
    MReturn (scs, sheap, ssto, pc')

  | AssignCallLab (params, exp,x,f)->
    (match SecStore.get_safe ssto x with
       | Some x_lvl -> 
          if (SL.leq (check_pc pc) x_lvl) then
            let scs'=SecCallStack.push scs (SecCallStack.Intermediate (pc,ssto,x)) in
            let lvls = List.map (expr_lvl ssto) exp in
            let pvs = List.combine params lvls in
            let ssto_aux = SecStore.create pvs in
            MReturn (scs', sheap, ssto_aux, [check_pc pc])
          else 
            MFail ((scs, sheap, ssto, pc), ("Pc bigger than x in AssignCall"))
       | None ->
          let scs'=SecCallStack.push scs (SecCallStack.Intermediate (pc,ssto,x)) in
            let lvls = List.map (expr_lvl ssto) exp in
            let pvs = List.combine params lvls in
            let ssto_aux = SecStore.create pvs in
            MReturn (scs', sheap, ssto_aux, [check_pc pc]))
      
    

  | NewLab (x, loc) ->
    let pc_lvl = check_pc pc in
    SecHeap.create_object sheap loc pc_lvl;
    SecStore.set ssto x pc_lvl;
    MReturn (scs, sheap, ssto, pc)

  | UpgVarLab (x,lev)->
      let pc_lvl= check_pc pc in
      (match SecStore.get_safe ssto x with
       | Some x_lvl ->
         if (SL.leq pc_lvl x_lvl ) then (
           SecStore.set ssto x (SL.lub lev pc_lvl);
           print_string ("SECSTORE = "^ x ^" <-"^ (SL.str (SL.lub lev pc_lvl)) ^ "\n");
           MReturn (scs, sheap, ssto, pc)
         ) else MFail ((scs, sheap, ssto, pc), ("Illegal UpgVarLab"))
       | None ->
         SecStore.set ssto x (SL.lub lev pc_lvl);
         print_string ("SECSTORE = "^ x ^" <-"^ (SL.str (SL.lub lev pc_lvl)) ^ "\n");
         MReturn (scs, sheap, ssto, pc))

  | UpgObjectLab (loc, e_o, lvl) -> (*UpgObjLab <- mudar*)
    let lev_o = expr_lvl ssto e_o in
    let lev_ctx = SL.lubn [lev_o;(check_pc pc)] in
    (match SecHeap.get_object_lvl sheap loc with
     | Some lev ->
       if SL.leq lev_ctx lev then (
         SecHeap.upg_object_lvl sheap loc (SL.lub lvl lev_ctx);
         MReturn (scs, sheap, ssto, pc))
       else
         MFail((scs,sheap,ssto,pc), "Illegal P_Val Upgrade")
     | None -> raise (Except "Internal Error"))


  | UpgStructLab (loc, e_o, lvl) -> (*StructLevel*)
    let lev_o = expr_lvl ssto e_o in
    let lev_ctx = SL.lub lev_o (check_pc pc) in
    (match SecHeap.get_struct_lvl sheap loc with
     | Some lev ->
       if SL.leq lev_ctx lev then (
         SecHeap.upg_struct_lvl sheap loc (SL.lub lvl lev_ctx);
         MReturn (scs, sheap, ssto, pc))
       else
         MFail((scs,sheap,ssto,pc), "Illegal P_Val Upgrade")
     | None -> raise (Except "Internal Error"))



  | UpgPropValLab (loc, field, e_o, e_f,  lvl) ->
    let lev_o = expr_lvl ssto e_o in
    let lev_f = expr_lvl ssto e_f in
    let lev_ctx = SL.lubn [lev_o ;lev_f;(check_pc pc)] in
    (match SecHeap.get_field sheap loc field with
     | Some (_, prop_val_lvl) ->
       if SL.leq lev_ctx prop_val_lvl then (
         SecHeap.upg_prop_val_lvl sheap loc field (SL.lub lvl lev_ctx);
         MReturn (scs, sheap, ssto, pc))
       else MFail((scs,sheap,ssto,pc), "Illegal P_Val Upgrade")
     | None -> raise (Except "Internal Error"))


  | UpgPropExistsLab (loc,field, e_o, e_f, lvl) ->
    let lev_o = expr_lvl ssto e_o in
    let lev_f = expr_lvl ssto e_f in
    let lev_ctx = SL.lubn [lev_o ;lev_f;(check_pc pc)] in
    (match SecHeap.get_field sheap loc field with
       Some (prop_exists_lvl, _) ->
       if SL.leq lev_ctx prop_exists_lvl then (
         SecHeap.upg_prop_exists_lvl sheap loc field (SL.lub lvl lev_ctx);
         MReturn (scs, sheap, ssto, pc))
       else MFail((scs,sheap,ssto,pc), "Illegal P_Exists Upgrade")
     |None -> raise (Except "Internal Error"))

  | FieldLookupLab (x,loc,field, e_o, e_f) ->
    let lev_o = expr_lvl ssto e_o in
    let lev_f = expr_lvl ssto e_f in
    let lev_ctx = SL.lubn [lev_o ;lev_f;(check_pc pc)] in
    let lev_x = Option.default lev_ctx (SecStore.get_safe ssto x) in
    if (SL.leq lev_ctx lev_x) then (
      match  SecHeap.get_field sheap loc field with
      | Some (_, prop_val_lvl) ->
        let lub = SL.lub lev_ctx prop_val_lvl  in
        SecStore.set ssto x lub;
        MReturn (scs,sheap,ssto,pc)
      | None -> raise (Except "Internal Error"))
    else
      MFail((scs,sheap,ssto,pc), "Illegal Field Lookup")

  | FieldDeleteLab (loc, field, e_o, e_f) ->
    let lev_o = expr_lvl ssto e_o in
    let lev_f = expr_lvl ssto e_f in
    let lev_ctx = SL.lubn [lev_o ;lev_f;(check_pc pc)] in
    (match SecHeap.get_field sheap loc field with
     | Some (prop_exists_lvl , _) ->
       if (SL.leq lev_ctx prop_exists_lvl) then (
         if SecHeap.delete_field sheap loc field then
           MReturn (scs,sheap,ssto,pc)
         else raise (Except "Internal Error"))
       else MFail((scs,sheap,ssto,pc), "Illegal Field Delete")
     | None -> raise (Except "Internal Error"))

  | FieldAssignLab ( loc, field, e_o, e_f, exp) ->
    let lev_o = expr_lvl ssto e_o in
    let lev_f = expr_lvl ssto e_f in
    let lev_ctx = SL.lubn [lev_o; lev_f; (check_pc pc)] in
    let lev_exp = expr_lvl ssto exp in
    (match SecHeap.get_field sheap loc field with
     | Some (prop_exists_lvl,prop_val_lvl) ->
       if (SL.leq lev_ctx prop_val_lvl) then (
         SecHeap.upg_prop_val_lvl sheap loc field (SL.lub lev_exp lev_ctx);
         MReturn (scs,sheap,ssto,pc))
       else MFail((scs,sheap,ssto,pc), "Illegal Field Assign")

     | None ->
       let lev_struct = SecHeap.get_struct_lvl sheap loc in
       (match lev_struct with
        | Some lev_struct ->
          if (SL.leq lev_ctx lev_struct) then (
            if SecHeap.new_sec_prop sheap loc field lev_ctx (SL.lub lev_exp lev_ctx) then
              MReturn (scs, sheap, ssto, pc)
            else raise (Except "Internal Error"))
          else MFail((scs,sheap,ssto,pc), "Illegal Field Assign")
        | None -> raise (Except "Internal Error")))

  | SetTopLab st ->
    SL.setTop (SL.parse_lvl st);
    MReturn (scs, sheap, ssto, pc)

  | AllowFlowLab (st1, st2) ->
    SL.addFlow (SL.parse_lvl st1) (SL.parse_lvl st2);
    print_string (">AllowFlow: " ^ st1 ^" -> {" ^ st2 ^ "}\n");
    MReturn (scs, sheap, ssto, pc)

let initial_monitor_state (): state_t =
  let sheap = SecHeap.create () in
  let ssto = SecStore.create [] in
  let scs = SecCallStack.create () in
  (scs, sheap, ssto, [(SL.get_low ())])


let parse_lvl = SL.parse_lvl

end 

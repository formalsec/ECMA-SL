(*Each monitor is independent of the other ones*)
exception Except of string

type monitor_return = | MReturn of ( SecCallStack.t * SecHeap.t * SecStore.t * SecLevel.t list )
                      | MFail of ( SecCallStack.t * SecHeap.t * SecStore.t * SecLevel.t list * string)



let print_pc (pc : SecLevel.t list) =
  print_string "[ M - STACK ]";
  let aux= List.rev pc in
  print_string ((String.concat ":: " (List.map SecLevel.str aux))^"\n")

let add_pc (pc : SecLevel.t list) (lvl : SecLevel.t) : SecLevel.t list=
  let aux= List.rev pc in
  let pc'=  [lvl] @ aux in
  List.rev pc'

let pop_pc (pc : SecLevel.t list) : SecLevel.t list =
  let pc'= List.rev pc in
  match pc' with
  |[] -> raise(Except "PC list is empty!")
  |l::ls'-> List.rev ls'

let check_pc (pc : SecLevel.t list) : SecLevel.t =
  let pc'= List.rev pc in
  match pc' with
  | s::ss'-> s
  | _ -> raise(Except "PC list is empty!")

let rec expr_lvl (ssto:SecStore.t) (exp:Expr.t) : SecLevel.t =
  (*Criar lub entre lista de variaveis*)
  let vars = Expr.vars exp in
  List.fold_left  (SecLevel.lub) SecLevel.Low  (List.map (SecStore.get ssto) vars)


let rec eval_small_step (prog:Prog.t) (scs:SecCallStack.t)  (sheap:SecHeap.t) (ssto:SecStore.t) (pc:SecLevel.t list) (tl:SecLabel.t) (verbose:bool): monitor_return =
  (if (verbose)
   then print_string ("[ M ]  "^(SecLabel.str tl)^"  \n")
  );
             (*
            No-Sensitive-Upgrade
            *)

  match tl with
  | EmptyLab ->
    MReturn (scs,sheap,ssto,pc)

  | MergeLab ->
    let pc' = pop_pc pc in
    (if verbose then  print_pc(pc'));
    MReturn (scs,sheap, ssto,pc')

  | ReturnLab e ->
    let lvl = expr_lvl ssto e in
    let lvl_f = SecLevel.lub lvl (check_pc pc) in
    let (frame, scs') = SecCallStack.pop scs in
    (match frame with
     | Intermediate (pc',ssto', x) ->  eval_small_step prog scs' sheap ssto' pc' (SecLabel.UpgVarLab (x,lvl_f)) verbose
     | Toplevel -> MReturn (scs', sheap, ssto, pc))

  | UpgVarLab (x,lev)->
    let pc_lvl= check_pc pc in
    let x_lvl = SecStore.get ssto x in
    if (SecLevel.leq x_lvl pc_lvl) then (
      SecStore.set ssto x (SecLevel.lub lev pc_lvl);
      MReturn (scs, sheap, ssto, pc)
    ) else MFail (scs, sheap, ssto, pc, ("NSU Violation - UpgVarLab: " ^ x ^ " " ^ (SecLevel.str lev)))

  | AssignLab (var, exp)->
    let lvl=expr_lvl ssto exp in
    let pc_lvl= check_pc pc in
    (try (let var_lvl = Hashtbl.find ssto var in
          if (SecLevel.leq var_lvl pc_lvl) then(
            print_string (SecLevel.str lvl);
            SecStore.set ssto var (SecLevel.lub lvl  pc_lvl);
            (if verbose then print_string ("[ M ] "^var^ " <- "^(SecLevel.str (SecLevel.lub lvl pc_lvl))^"\n"));
            MReturn (scs, sheap, ssto, pc))

          else (raise(Except "MONITOR BLOCK - Invalid Assignment "))
         )
     with Not_found -> 	SecStore.set ssto var (SecLevel.lub lvl  pc_lvl);
       eval_small_step prog scs sheap ssto pc (SecLabel.AssignLab (var,exp)) verbose
    )

  (*| OutLab (lev,exp) ->
    let lvl_pc= check_pc pc in
    let lvl_exp = expr_lvl ssto exp in
    if (Level.leq (Level.lub lvl_exp lvl_pc) lev) then
    MReturn (scs, sheap, ssto, pc)
    else
    MFail (scs, sheap, ssto, pc, ("NSU Violation - OutLab: " ^ (SecLevel.str lev) ^ " " ^ (Expr.str exp)))
  *)
  | BranchLab (exp,st) ->
    let lev= expr_lvl ssto exp in
    let pc_lvl = check_pc pc in
    let pc' = add_pc pc (SecLevel.lub lev pc_lvl) in
    MReturn (scs, sheap, ssto, pc')

  | AssignCallLab (exp,x,f)->
    let scs'=SecCallStack.push scs (SecCallStack.Intermediate (pc,ssto,x)) in
    let lvls = List.map (expr_lvl ssto) exp in
    let pvs = List.combine (Prog.get_params prog f) lvls in
    let ssto_aux = SecStore.create_store pvs in
    MReturn (scs', sheap, ssto_aux, [check_pc pc])

  | UpgStructValLab (loc,lvl) ->
    (*Need to add NSU conditions*)
    SecHeap.upg_struct_val sheap loc lvl;
    MReturn (scs, sheap, ssto, pc)

  | UpgStructExistsLab (loc,lvl) ->
    (*Need to add NSU conditions*)
    SecHeap.upg_struct_exists sheap loc lvl;
    MReturn (scs, sheap, ssto, pc)

  | UpgPropValLab (loc,prop,lvl) ->
    (*Need to add NSU conditions*)
    SecHeap.upg_prop_val sheap loc prop lvl;
    MReturn (scs, sheap, ssto, pc)

  | UpgPropExistsLab (loc,prop,lvl) ->
    (*Need to add NSU conditions*)
    SecHeap.upg_prop_exists sheap loc prop lvl;
    MReturn (scs, sheap, ssto, pc)

  | FieldLookupLab (x,loc,field, e_o, e_f) ->
    let lev_o = expr_lvl ssto e_o in
    let lev_f = expr_lvl ssto e_f in
    let lev_ctx = SecLevel.lubn [lev_o ;lev_f;(check_pc pc)] in
    let lev_x = SecStore.get ssto x in
    if (SecLevel.leq lev_ctx lev_x) then (
      match  SecHeap.get_field sheap loc field with
      | Some (_, lev_fv) ->
        let lub = SecLevel.lub lev_ctx lev_fv  in
        SecStore.set ssto x lub;
        MReturn (scs,sheap,ssto,pc)
      | None -> raise (Except "Internal Error"))
    else
      MFail(scs,sheap,ssto,pc, "Illegal Lookup")

  | FieldDeleteLab (loc, field, e_o, e_f) ->
    let lev_o = expr_lvl ssto e_o in
    let lev_f = expr_lvl ssto e_f in
    let lev_ctx = SecLevel.lubn [lev_o ;lev_f;(check_pc pc)] in
    (match SecHeap.get_field sheap loc field with
     | Some (lev_ef , _) ->
       if (SecLevel.leq lev_ctx lev_ef) then (
         if SecHeap.delete_field sheap loc field then
           MReturn (scs,sheap,ssto,pc)
         else raise (Except "Internal Error"))
       else MFail(scs,sheap,ssto,pc, "Illegal Delete")
     | None -> raise (Except "Internal Error"))

  | FieldAssignLab ( loc, field, e_o, e_f, exp) ->
    let lev_o = expr_lvl ssto e_o in
    let lev_f = expr_lvl ssto e_f in
    let lev_ctx = SecLevel.lubn [lev_o; lev_f; (check_pc pc)] in
    (match SecHeap.get_field sheap loc field with
     | Some (lev_ef,lev_fv) ->
       if (SecLevel.leq lev_ctx lev_ef) then
         MReturn (scs,sheap,ssto,pc)  (*FALTA ACABAR ISTO - FOI SO PARA COMPILAR*)
       else raise (Except "Internal Error")

     |None -> raise (Except "Internal Error"))



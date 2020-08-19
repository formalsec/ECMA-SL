(*Each monitor is independent of the other ones*)
exception Except of string

type monitor_return = | MReturn of ( SecCallStack.t * SecHeap.t * SecStore.t * Level.t list )
                      | MFail of ( SecCallStack.t * SecHeap.t * SecStore.t * Level.t list * string)



let print_pc (pc: Level.t list) =
  print_string "[ M - STACK ]";
  let aux= List.rev pc in
  print_string ((String.concat ":: " (List.map Level.str aux))^"\n")

let add_pc (pc:Level.t list) (lvl : Level.t) : Level.t list=
  let aux= List.rev pc in
  let pc'=  [lvl] @ aux in
  List.rev pc'

let pop_pc (pc: Level.t list) : Level.t list =
  let pc'= List.rev pc in
  match pc' with
  |[] -> raise(Except "PC list is empty!")
  |l::ls'-> List.rev ls'

let check_pc (pc: Level.t list):(Level.t) =
  let pc'= List.rev pc in
  match pc' with
  | s::ss'-> s
  | _ -> raise(Except "PC list is empty!")

let rec expr_lvl (ssto:SecStore.t) (exp:Expr.t) : Level.t =
  (*Criar lub entre lista de variaveis*)
  let vars = Expr.vars exp in
  let vars_lvl =
  match vars with
  | [] -> SecLevel.Low
  | _ -> List.fold_left  (SecLevel.lub) acc (List.map (SecStore.get_store ssto) vars);
    acc

let rec eval_small_step (prog:Prog.t) (scs:SecCallStack.t)  (sheap:SecHeap.t) (ssto:SecStore.t) (pc:Level.t list) (tl:TLabel.t) (verbose:bool): monitor_return =
  (if (verbose)
   then print_string ("[ M ]  "^(TLabel.str tl)^"  \n")
  );
             (*
            No-Sensitive-Upgrade
            *)

  (match tl with
   | EmptyLab -> MReturn (scs,sheap,ssto,pc)
   | MergeLab -> let pc' = pop_pc pc in
     (if verbose then  print_pc(pc'));
     MReturn (scs,sheap, ssto,pc')

   | RetLab e ->
      let lvl = expr_lvl ssto e in
      let lvl_f = Level.lub lvl check_pc pc in
      let (frame, scs') = SecCallStack.pop scs in
      (match frame with
        | Intermediate (pc',ssto', x) ->  eval_small_step prog scs' sheap ssto' pc' (TLabel.UpgVarLab (x,lvl_f)) verbose
        | Toplevel -> MReturn (scs', sheap, ssto, pc))

   | UpgVarLab (x,lev)->
      let pc_lvl= check_pc pc in
      let x_lvl = SecStore.get_lvl ssto x in
        if (Level.leq x_lvl pc_lvl) then (
          SecStore.set_store ssto x (Level.lub lev pc_lvl);
          MReturn (scs, sheap, ssto, pc)
        ) else MFail (scs, sheap, ssto, pc, ("NSU Violation - UpgVarLab: " ^ x ^ " " ^ (SecLevel.str lev)))

   | OutLab (lev,exp) ->
     let lvl_pc= check_pc pc in
     let lvl_exp = expr_lvl ssto exp in
     if (Level.leq (Level.lub lvl_exp lvl_pc) lev) then
       MReturn (scs, sheap, ssto, pc)
     else
       MFail (scs, sheap, ssto, pc, ("NSU Violation - OutLab: " ^ (SecLevel.str lev) ^ " " ^ (Expr.str exp)))

   | BranchLab (exp) ->
      let lev= expr_lvl ssto exp in
      let pc_lvl = check_pc pc in
      let pc' = add_pc pc lev in
      MReturn (scs, sheap, ssto, pc')

   | CallLab (exp,x,f)->
     let scs'=SecCallStack.push scs (SecCallStack.Intermediate (pc,ssto,x)) in
     let lvls = List.map (expr_lvl ssto) exp in
     let pvs = List.combine (Prog.get_params prog f) lvls in
     let ssto_aux = SecStore.create_store pvs in
     MReturn (scs', sheap, ssto_aux, [check_pc pc])


   | UpgPropExistsLab (loc,prop,lvl) ->
     SecHeap.upg_prop_exist_lab sheap loc prop lvl;
     MReturn (scs, sheap, ssto, pc)

   | UpgStructLab (loc,lvl) ->
     SecHeap.upg_prop_exist_lab sheap loc prop lvl;
     MReturn (scs, sheap, ssto, pc)

   | UpgPropValLab (loc,str,lvl) ->
     MReturn (scs, sheap, ssto, pc)

  )

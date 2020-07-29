(*Each monitor is independent of the other ones*)
exception Except of string

type monitor_return = MReturn of (Level.t list *SecStore.t * SecCallStack.t)



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



let rec eval_expr (ssto:SecStore.t) (exp:Expr.t) : Level.t =
  match exp with
  | Val v-> Level.Low
  | Var x -> SecStore.get_store ssto x
  | UnOp (op,e) -> eval_expr ssto e
  | BinOp (op,e1,e2) -> let level_e1 = eval_expr ssto e1 in
    let level_e2 = eval_expr ssto e2 in
    Level.parse_lvl (Level.lub (Level.str level_e1) (Level.str level_e2))


let rec eval_small_step (prog:Prog.t) (scs:SecCallStack.t) (ssto:SecStore.t) (pc:Level.t list) (tl:TLabel.t) (verbose:bool): monitor_return =
  (if (verbose)
   then print_string ("[ M ]  "^(TLabel.str tl)^"  \n")
  );


 (*
No-Sensitive-Upgrade
*)


  (match tl with
   |EmptyLab -> MReturn (pc, ssto,scs)
   |MergeLab -> let pc' = pop_pc pc in
     (if verbose then  print_pc(pc'));
     MReturn (pc', ssto,scs)


   |RetLab e ->  (let lvl = eval_expr ssto e in
                  let lvl_f = Level.parse_lvl (Level.lub (Level.str lvl) (Level.str (check_pc pc))) in
                  let (f, scs') = SecCallStack.pop scs in
                  match f with
                  | Intermediate (pc',ssto', var) ->  eval_small_step prog scs' ssto' pc' (TLabel.AsgnLevLab (var,lvl_f)) verbose
                  | Toplevel -> MReturn (pc, ssto,scs)
                 )


   |UpgradeLab (var,lev) -> eval_small_step prog scs ssto pc (AsgnLevLab(var, (Level.parse_lvl(lev)))) verbose


   |AsgnLevLab (var,lev)-> let pc_lvl= check_pc pc in
     (try (let var_lvl = Hashtbl.find ssto var in
           if (Level.leq (Level.str var_lvl) (Level.str pc_lvl)) then(
             print_string (Level.str lev);
             SecStore.set_store ssto var (Level.parse_lvl (Level.lub (Level.str lev) (Level.str pc_lvl)));
             (if verbose then print_string ("[ M ] "^var^ " <- "^(Level.lub (Level.str lev) (Level.str pc_lvl))^"\n"));
             MReturn (pc, ssto,scs))

           else (raise(Except "MONITOR BLOCK - Invalid Assignment "))
          )
      with Not_found -> 	SecStore.set_store ssto var (Level.parse_lvl (Level.lub (Level.str lev) (Level.str pc_lvl)));
        (if verbose then print_string ("[ M ] "^var^ " <- "^(Level.str lev)^"\n"));
        eval_small_step prog scs ssto pc (TLabel.AsgnLevLab (var,lev)) verbose
     )




   |AsgnLab (var, exp) ->	let lvl=eval_expr ssto exp in


     eval_small_step prog scs ssto pc (TLabel.AsgnLevLab (var,lvl)) verbose

   |OutLab (lev,exp) ->  let lvl_pc= check_pc pc in
     let lvl_lev = (Level.parse_lvl lev) in
     let lvl_exp = eval_expr ssto exp in
     if(Level.leq (Level.str lvl_lev) (Level.lub (Level.str (lvl_exp)) (Level.str (lvl_pc)))) then
       MReturn (pc, ssto, scs)
     else
       raise(Except "MONITOR BLOCK - Output levels invalid")

   |BranchLab (exp, stms) -> let lev= eval_expr ssto exp in
     (if verbose then print_string ("[M] - Branch eval = " ^ (Level.str lev)));
     let pc_lvl = check_pc pc in
     if(Level.leq (Level.str lev) (Level.str pc_lvl) ) then(
       let pc' = add_pc pc lev in
       (if verbose then ( print_string ("[ M ] Level added to stack"^ " <- "^(Level.str lev)^"\n");
                          print_pc(pc')));
       MReturn (pc', ssto,scs))
     else (raise(Except "MONITOR BLOCK - Low branch in High guard"))


   |CallLab (exp,x,f)->let scs'=SecCallStack.push scs (SecCallStack.Intermediate (pc,ssto,x)) in
     let lvls = (List.map (eval_expr ssto) exp) in
     let pvs =( List.combine (Prog.get_params prog f) lvls) in
     let ssto_aux = SecStore.create_store pvs in
     MReturn (pc,ssto_aux,scs')
  )

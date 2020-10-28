type base_t = 
  | Var of string 
  | Loc of string 

type obj_exp_t = (base_t * string list)

type t = 
    ShowVal   of obj_exp_t 
  | ShowObj   of obj_exp_t 
  | ShowStore 
  | ShowHeap
  | Continue 

let eval (heap : Heap.t) (sto : Store.t) (e : obj_exp_t) : Val.t = 
  let (b, ps) = e in 
  let v_b = 
    match b with 
      | Var x -> Store.get sto x 
      | Loc l -> Val.Loc l in 
  List.fold_left
    (fun ac p -> 
      match ac with 
        | Val.Loc l -> 
          let v = Heap.get_field heap l p in
          Option.default Val.Null v 
        | _ -> raise (Failure "Base value is not a location")
    ) v_b ps 

let show_inspector_dialog () : unit = 
  let dialog_str = "Options: \n" 
                    ^ "\t1: val <expr>\n"
                    ^ "\t2: store\n"
                    ^ "\t3: heap\n"
                    ^ "\t4: obj <expr>\n"
                    ^ "\t5: continue\n" in 
  Printf.printf "%s" dialog_str 

let parse_base (s : string) : base_t = 
  match Loc.parse_loc s with 
    | Some l -> Loc l 
    | _ -> Var s 

let parse_cmd (str : string) : t option = 
  let strs = String.split_on_char ' ' str in 
  match strs with 
    | [] -> Some Continue 
    | [cmd; e_str ] when cmd = "val" || cmd = "obj" -> 
      let e_strs = String.split_on_char '.' e_str in 
      (match e_strs with 
        | y::ps -> 
            let base_y = parse_base y in
            if cmd = "val" 
              then Some (ShowVal (base_y, ps))
              else Some (ShowObj (base_y, ps))
        | _ -> None)
    | ["store"] -> Some ShowStore
    | ["heap"] -> Some ShowHeap
    | ["continue"] -> Some Continue
    | _ -> None 

let rec inspector (heap : Heap.t) (sto : Store.t) : unit = 
  let f () = inspector heap sto in 
  show_inspector_dialog ();
  let line = read_line () in
  let answer = parse_cmd line in 
  match answer with 
    | Some (ShowVal e) ->
      let v = eval heap sto e in
      Printf.printf "Val: %s\n" (Val.str v); 
      f ()  
    | Some (ShowObj e) -> 
      let v = eval heap sto e in
      (match v with 
      | Val.Loc l ->  
        (match Heap.get heap l with
        | Some o -> Printf.printf "Obj: %s\n" (Object.str o)
        | None   -> Printf.printf "Obj: %s\n" "Non-Existent")
      | _ -> Printf.printf "Provided Location is not an object. Try again!\n");
      f ()
    | Some ShowStore -> 
      Printf.printf "%s" (Store.str sto); f () 
    | Some ShowHeap -> 
      Printf.printf "%s" (Heap.str heap); f () 
    | Some Continue -> () 
    | None -> 
      Printf.printf "%s" "Wrong INPUT. Try again.\n"; f () 
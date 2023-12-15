open Core
module Object = S_object_ite_no_branch_undef

type encoded_pct = Encoding.Expression.t
type obj = Object.t
type t = { parent : t option; map : (Loc.t, obj) Hashtbl.t }

let create () : t = { parent = None; map = Hashtbl.create (module String) }

let clone (h : t) : t =
  { parent = Some h; map = Hashtbl.create (module String) }

let insert (h : t) (obj : obj) : Loc.t =
  let loc = Loc.newloc () in
  Hashtbl.set h.map ~key:loc ~data:obj;
  loc

let remove (h : t) (l : Loc.t) : unit = Hashtbl.remove h.map l
let set (h : t) (key : Loc.t) (data : obj) : unit = Hashtbl.set h.map ~key ~data

let rec get (h : t) (l : Loc.t) : obj option =
  let result = Hashtbl.find h.map l in
  match result with
  | Some o -> result
  | None -> (
      let obj = Option.bind h.parent ~f:(fun h -> get h l) in
      match obj with
      | Some o ->
          let o' = Object.clone o in
          set h l o';
          Some o'
      | None -> None)

let mk_ite (e1 : Expr.t) (e2 : Expr.t) (e3 : Expr.t) : Expr.t =
  Expr.TriOpt (Operators.ITE, e1, e2, e3)
let mk_not (e : Expr.t) : Expr.t = Expr.UnOpt (Operators.Not, e)
let mk_bool (b : bool) : Expr.t = Expr.Val (Val.Bool false)

let mk_and (e1 : Expr.t) (e2 : Expr.t) : Expr.t = Expr.BinOpt(Operators.Log_And, e1, e2)
let mk_or (e1 : Expr.t) (e2 : Expr.t) : Expr.t = Expr.BinOpt(Operators.Log_Or, e1, e2)


let apply_op_get (h : t) (loc : Expr.t) (cond : Expr.t) (left : Expr.t)
    (right : Expr.t) (solver : Batch.t)
    (op : Expr.t -> encoded_pct list -> Expr.t) (pc : encoded_pct list)
    (store : S_store.t) : 'a =
  let encoded_guard = Translator.translate cond in
  let pc_left = encoded_guard :: pc in
  let encoded_guard = Translator.translate (mk_not cond) in
  let pc_right = encoded_guard :: pc in
  let cs = Batch.check solver in

  match (cs pc_left, cs pc_right) with
  | true, true ->
      let vl = op left pc_left in
      let vr = op right pc_right in
      mk_ite cond vl vr
  | true, false -> op left pc_left
  | false, true -> op right pc_right
  | _ -> failwith "Apply op error."


let apply_op_set (h : t) (loc : Expr.t) (cond : Expr.t) (left : Expr.t)
    (right : Expr.t) (solver : Batch.t)
    (op :
      Expr.t ->
      encoded_pct list ->
      encoded_pct option ->
      t ->
      (t * encoded_pct list) list) (pc : encoded_pct list) (store : S_store.t) :
    (t * encoded_pct list) list =
  let encoded_guard_l = Translator.translate cond in
  let pc_l = encoded_guard_l :: pc in
  let encoded_guard_r = Translator.translate (mk_not cond) in
  let pc_r = encoded_guard_r :: pc in
  let cs = Batch.check solver in

  match (cs pc_l, cs pc_r) with
  | true, true ->
      op left pc_l (Some encoded_guard_l) (clone h)
      @ op right pc_r (Some encoded_guard_r) (clone h)
  | true, false -> op left pc_l (Some encoded_guard_l) (clone h)
  | false, true -> op right pc_r (Some encoded_guard_r) (clone h)
  | _ -> failwith "No path is valid in Set."

let rec assign_obj_fields (h : t) (loc : Expr.t) (solver : Batch.t)
    (pc : encoded_pct list) (store : S_store.t) : 'a =
  match loc with
  | Expr.Val (Val.Loc l) -> (
      let obj = get h l in
      match obj with
      | None -> failwith "Object not found."
      | Some o -> Expr.NOpt (Operators.ListExpr, Object.get_fields o))
  | Expr.TriOpt (Operators.ITE, cond, left, right) ->
      let op l pc = assign_obj_fields h l solver pc store in
      apply_op_get h loc cond left right solver op pc store
  | Expr.Val (Val.Symbol "undefined") -> failwith "impossible"
  | _ -> assert false

let rec assign_obj_to_list (h : t) (loc : Expr.t) (solver : Batch.t)
    (pc : encoded_pct list) (store : S_store.t) : 'a =
  match loc with
  | Expr.Val (Val.Loc l) -> (
      let obj = get h l in
      match obj with
      | None -> failwith "Object not found."
      | Some o ->
          let ret =
            Expr.NOpt
              ( Operators.ListExpr,
                List.map (Object.to_list o) ~f:(fun (f, v) ->
                    Expr.NOpt (Operators.TupleExpr, [ f; v ])) )
          in
          ret)
  | Expr.TriOpt (Operators.ITE, cond, left, right) ->
      let op l pc = assign_obj_to_list h l solver pc store in
      apply_op_get h loc cond left right solver op pc store
  | Expr.Val (Val.Symbol "undefined") -> failwith "impossible"
  | _ -> assert false

let rec has_field_aux (h : t) (loc : Expr.t) (field : Expr.t)
    (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    Expr.t =
  match loc with
  | Expr.Val (Val.Loc l) ->
      Option.value_map (get h l) ~default:(mk_bool false) ~f:(fun o ->
          Object.has_field o field)
  | Expr.TriOpt (Operators.ITE, cond, left, right) ->
      let op l pc = has_field_aux h l field solver pc store in
      apply_op_get h loc cond left right solver op pc store
  | Expr.Val (Val.Symbol "undefined") -> mk_bool false
  | _ -> assert false

let has_field (h : t) (loc : Expr.t) (field : Expr.t)
    (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    (t * encoded_pct list * Expr.t) list =
  [ (h, [], has_field_aux h loc field solver pc store) ]

let rec get_field_aux ?(guard = None) (heap : t) (loc : Expr.t) (field : Expr.t)
  (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) 
  
  (acc : (Expr.t * Expr.t option * Expr.t option) list * (Expr.t * Expr.t) list) :
  (Expr.t * Expr.t option * Expr.t option) list * (Expr.t * Expr.t) list =
  (* 
    o0 = {p : o1, q : o2}
    o1 = {y1 : 1, y2 : 2}
    o2 = {v1 : 3, v2 : 4}
    
    x = o[#s] => ITE(#s == p, o1, o2)
    z = x[#w]

    (#s == p) <=> cond1
    (#s == q) <=> cond2

    (
      ( (#s == "p") AND ( (#w != y1) AND (#w != y2) ) )
      OR
      ( (#s == "q") AND ( (#w != v1) AND (#w != v2) ) )
    ) => undefined
    
    <=>

    (
      (cond1 AND get_pc1)
      OR
      (cond2 AND get_pc2)
    )

    Problema -> não tenho a cond2, porque ela esta encoded na pc e no
    ITE gerado no acesso a o0 [ITE(#s == p, o1, o2)]


    Solução:

    get no objeto gera: 
    x = o[#s] => 
      branch sem undefined: ITE(#s == p, o1, ITE(#s == q, o2, undefined)), pc: #s == p || #s == q
      branch com undefined: undef, pc: #s != p && #s != q


    z = x[#w] =>
      cond1 AND get_pc1
      OR
      cond2 AND get_pc2

   
  *)
  
  
  match loc with
  | Expr.Val (Val.Loc l) -> (
      let obj = get heap l in
      match obj with
      | None -> failwith "Object not found."
      | Some o -> 
        let conc, undef = Object.get o field solver pc store in

        let conc' = 
          let v1, pc' = conc in v1, pc', guard in

        let acc1, acc2 = acc in
        match undef with
        (* case Some (v, None) is impossible. Check create_ite on obj *)
        | Some (v, Some pc') -> 
          let pc' = 
            match guard with 
            | Some guard -> mk_and guard pc' 
            | None -> pc' 
          in conc' :: acc1, (v, pc') :: acc2
        | _ -> conc' :: acc1, acc2
    )
  | Expr.TriOpt (Operators.ITE, cond, left, right) ->
      let op l pc guard = get_field_aux ~guard heap l field solver pc store in
      
      let encoded_guard = Translator.translate cond in
      let pc_left = encoded_guard :: pc in
      let encoded_guard = Translator.translate (mk_not cond) in
      let pc_right = encoded_guard :: pc in
      let cs = Batch.check solver in
      (match (cs pc_left, cs pc_right) with
      | true, true -> 
          let acc' = op left pc_left (Some cond) acc in
          let acc' = op right pc_right None acc' in
          acc'
      | true, false -> op left pc_left (Some cond) acc
      | false, true -> op right pc_right None acc
      | _ -> failwith "Apply op error.") 
  | Expr.Val (Val.Symbol "undefined") ->
      let e =
        Printf.sprintf "Get failed: |field:%s| \"undefined\"" (Expr.str field)
      in
      failwith e
  | _ -> assert false 

let get_field (heap : t) (loc : Expr.t) (field : Expr.t)
    (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    (t * encoded_pct list * Expr.t option) list =
  let acc_conc, acc_undef = 
    get_field_aux heap loc field solver pc store ([], []) in
  
  let false_e = mk_bool false in
  let undef = Expr.Val (Val.Symbol "undefined") in
  let pc_undef = List.fold acc_undef ~init:(false_e) ~f:(fun acc (_, pc') -> 
    if Expr.equal acc false_e then
      pc'
    else
      mk_or pc' acc
  ) in

  (* if equals false then there are no undef values, create pct for conc values *)
  let pc_conc, v_ite = 
    if Expr.equal pc_undef false_e then
      List.fold acc_conc ~init:(false_e, undef) 
        ~f:(fun (acc_pc, acc_v) (v, pc', guard) -> 
          let acc_pc = match pc' with
          | Some pc' ->
            if Expr.equal acc_pc false_e then
              pc'
            else
              mk_or pc' acc_pc
          | None -> acc_pc
          in
          let acc_v =
            match guard with
            | None -> v
            | Some guard ->
              mk_ite guard v acc_v

          in acc_pc, acc_v
        ) 
    else
      let v =       
        List.fold acc_conc ~init:(undef) 
          ~f:(fun acc_v (v, pc', guard) -> 
            let acc_v =
              match guard with
              | None -> v
              | Some guard ->
                mk_ite guard v acc_v

            in acc_v
          ) 
      in
      mk_not pc_undef, v
  in

  if Expr.equal (pc_undef) false_e then
    [(heap, [Translator.translate pc_conc], Some v_ite)]
  else
    [
      (clone heap, [Translator.translate pc_conc], Some v_ite);
      (clone heap, [Translator.translate pc_undef], None)
    ]

  



let set_field_exec (heap : t) (loc : Loc.t) (field : Expr.t) (v : 'a)
    (solver : Batch.t) (pc : encoded_pct list)
    (encoded_guard : encoded_pct option) (store : S_store.t) :
    (t * encoded_pct list) list =
  let obj = get heap loc in
  let res =
    Option.bind obj ~f:(fun o -> Some (Object.set o field v solver pc store))
  in
  match res with
  | None -> failwith ("set Return is never none. loc: " ^ loc)
  | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc) ] ->
          let pc = match encoded_guard with None -> pc | Some p -> p :: pc in
          set heap loc obj;
          [ (heap, pc) ]
      | _ ->
          List.map objs ~f:(fun (obj, pc) ->
              let heap' = clone heap in
              let pc =
                match encoded_guard with None -> pc | Some p -> p :: pc
              in
              set heap' loc obj;
              (heap', pc)))

let rec set_field_aux ?(encoded_guard = None) (heap : t) (loc : Expr.t)
    (field : Expr.t) (v : 'a) (solver : Batch.t)
    (pc : encoded_pct list) (store : S_store.t) : (t * encoded_pct list) list =
  match loc with
  | Expr.Val (Val.Loc l) ->
      set_field_exec heap l field v solver pc encoded_guard store
  | Expr.TriOpt (Operators.ITE, cond, left, right) ->
      let op l pc guard h =
        set_field_aux ~encoded_guard:guard h l field v solver pc store
      in
      apply_op_set heap loc cond left right solver op pc store
  | Expr.Val (Val.Symbol "undefined") -> failwith "Attempting to set undefined."
  | _ -> assert false

let set_field (heap : t) (loc : Expr.t) (field : Expr.t) (v : 'a)
    (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    (t * encoded_pct list) list =
  set_field_aux heap loc field v solver pc store

let delete_field_exec (heap : t) (loc : Loc.t) (field : Expr.t)
    (solver : Batch.t) (pc : encoded_pct list)
    (encoded_guard : encoded_pct option) (store : S_store.t) :
    (t * encoded_pct list) list =
  let obj = get heap loc in
  let res =
    Option.bind obj ~f:(fun o -> Some (Object.delete o field solver pc store))
  in
  match res with
  | None -> failwith ("delete Return is never none. loc: " ^ loc)
  | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc) ] ->
          let pc = match encoded_guard with None -> pc | Some p -> p :: pc in
          set heap loc obj;
          [ (heap, pc) ]
      | _ ->
          List.map objs ~f:(fun (obj, pc) ->
              let heap' = clone heap in
              set heap' loc obj;
              let pc =
                match encoded_guard with None -> pc | Some p -> p :: pc
              in
              (heap', pc)))

let rec delete_field_aux ?(encoded_guard = None) (heap : t) (loc : Expr.t)
    (field : Expr.t) (solver : Batch.t) (pc : encoded_pct list)
    (store : S_store.t) : (t * encoded_pct list) list =
  match loc with
  | Expr.Val (Val.Loc l) ->
      delete_field_exec heap l field solver pc encoded_guard store
  | Expr.TriOpt (Operators.ITE, cond, left, right) ->
      let op l pc guard h =
        delete_field_aux ~encoded_guard:guard h l field solver pc store
      in
      apply_op_set heap loc cond left right solver op pc store
  | Expr.Val (Val.Symbol "undefined") -> failwith "del woops"
  | _ -> assert false

let delete_field (heap : t) (loc : Expr.t) (field : Expr.t)
    (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
    (t * encoded_pct list) list =
  delete_field_aux heap loc field solver pc store
(* let to_string (h : 'a t) (pp : 'a -> string) : string =
    "{ "
    ^ String.concat ~sep:", "
        (Hashtbl.fold h.map ~init:[] ~f:(fun ~key:n ~data:v acc ->
            Printf.sprintf "%s: %s" (Loc.str n) (S_object.str v pp) :: acc))
    ^ " }" *)

(* let to_string_with_glob (h : 'a t) (pp : 'a -> string) : string =
    let glob =
      Hashtbl.fold h.map ~init:None ~f:(fun ~key:_ ~data:obj acc ->
          match acc with
          | Some _ -> acc
          (* Keep this in sync with Compiler.ml function *)
          (* "compile_gvar" and "compile_glob_assign" *)
          | None -> S_object.get_concrete_field obj Common.global_var_compiled)
    in
    match glob with
    | Some l ->
        Printf.sprintf "{ \"heap\": %s, \"global\": %s }" (to_string h pp)
          (Val.str l)
    | None ->
        raise
          (Failure
            "Couldn't find the Object that contains only one property, named \
              \"global\".") *)

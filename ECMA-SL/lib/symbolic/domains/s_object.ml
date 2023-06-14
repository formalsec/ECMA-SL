open Core

type vt = Expr.t
type pct = Expr.t
type encoded_pct = Encoding.Expression.t

let counter = ref 0

module ExprHash = struct
  type t = Expr.t

  let equal (e1 : Expr.t) (e2 : Expr.t) = Expr.equal e1 e2
  let hash (e : Expr.t) = Hashtbl.hash e
  let t_of_sexp e = failwith "Not implemented."
  let sexp_of_t e = failwith "Not implemented"
  let compare (e1 : Expr.t) (e2 : Expr.t) = Hashtbl.hash e1 - Hashtbl.hash e2
end

module Expr_Hashtbl = Hashtbl.Make (ExprHash)

type 'a t = {
  concrete_fields : (String.t, 'a) Hashtbl.t;
  symbolic_fields : 'a Expr_Hashtbl.t;
}

let create () : 'a t =
  {
    concrete_fields = Hashtbl.create (module String);
    symbolic_fields = Hashtbl.create (module ExprHash);
  }

let clone (o : 'a t) : 'a t =
  {
    concrete_fields = Hashtbl.copy o.concrete_fields;
    symbolic_fields = Expr_Hashtbl.copy o.symbolic_fields;
  }

(* let clone (o : 'a t) : 'a t =
  let o' = create() in

  Hashtbl.iteri o.concrete_fields ~f:(fun ~key ~data ->
    Hashtbl.set o'.concrete_fields ~key:key ~data:data
  );
  Expr_Hashtbl.iteri o.symbolic_fields ~f:(fun ~key ~data ->
    Expr_Hashtbl.set o'.symbolic_fields ~key:key ~data:data
  );
  o' *)


let to_string (o : 'a t) (printer : 'a -> string) : string =
  let str_obj =
    Hashtbl.fold o.concrete_fields ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
        (if String.(ac <> "{ ") then ac ^ ", " else ac)
        ^ Printf.sprintf "\"%s\": %s" n (printer v))
    ^ "|||"
  in
  let str_obj =
    List.fold_left (Expr_Hashtbl.to_alist o.symbolic_fields)
      ~init:(str_obj ^ ", ") ~f:(fun acc (key, data) ->
        acc ^ Printf.sprintf "\"$symb_%s\": %s, " (Expr.str key) (printer data))
  in
  str_obj ^ " }"

let set_symbolic_field (o : 'a t) (key : vt) (data : 'a) : unit =
  Expr_Hashtbl.set o.symbolic_fields ~key ~data

let set_concrete_field (o : 'a t) (key : vt) (data : 'a) : unit =
  match key with
  | Expr.Val (Val.Str s) -> Hashtbl.set o.concrete_fields ~key:s ~data
  | _ -> failwith ("bad key: " ^ Expr.str key)

let has_concrete_key (o : 'a t) (key : string) : bool =
  let res = Hashtbl.find o.concrete_fields key in
  match res with Some _ -> true | None -> false

let concrete_to_list (o : 'a t) : (pct * 'a) list =
  let s_l = Hashtbl.to_alist o.concrete_fields in
  List.map s_l ~f:(fun (k, v) -> (Expr.Val (Val.Str k), v))

let get_symbolic_field (o : 'a t) (key : vt) : 'a option =
  Expr_Hashtbl.find o.symbolic_fields key

let get_concrete_field (o : 'a t) (key : string) : 'a option =
  Hashtbl.find o.concrete_fields key

let create_not_pct (l : (pct * 'a) list) (key : pct) (store : Sstore.t) : encoded_pct =
  if List.length l = 0 then Translator.translate (Expr.Val (Val.Bool true))
  else
    let expr =
      List.fold l ~init:(Expr.Val (Val.Bool true)) ~f:(fun acc (pc, _) ->
          let ne =
            Expr.UnOpt (Operators.Not, Expr.BinOpt (Operators.Eq, key, pc))
          in
          Expr.BinOpt (Operators.Log_And, ne, acc))
    in
    let expr = Reducer.reduce_expr store expr in
    Translator.translate expr

let create_object (o : 'a t) (key1 : pct) (key2 : pct) (store : Sstore.t) :
    'a t * encoded_pct option =
  let o' = clone o in
  let eq = Expr.BinOpt (Operators.Eq, key1, key2) in
  let eq = Reducer.reduce_expr store eq in
  let eq = Translator.translate eq in
  (o', Some eq)

let is_key_possible ?(b = false) (key1 : Expr.t) (key2 : Expr.t) (solver : Encoding.Batch.t)
    (pc : encoded_pct list) (store : Sstore.t): bool =
  let eq = Expr.BinOpt (Operators.Eq, key1, key2) in
  let eq = Reducer.reduce_expr store eq in
  let eq' = Translator.translate eq in
  let ret = Encoding.Batch.check_sat solver (eq' :: pc) in

  if b then (
    Printf.printf "\n\n";
    if not ret then
      List.iter pc ~f:(fun v ->
          Printf.printf "%s\n" (Encoding.Expression.to_string v));
    Printf.printf "create_object tested: %s, result: %b\n" (Expr.str eq) ret;
    Printf.printf "\n\n";
    ret)
  else ret

let set (o : 'a t) (key : vt) (data : 'a) (solver : Encoding.Batch.t)
    (pc : encoded_pct list) (store : Sstore.t) : ('a t * encoded_pct option) list =
  match key with
  | Expr.Val (Val.Str s) ->
      if has_concrete_key o s || Expr_Hashtbl.length o.symbolic_fields = 0 then
        let _ = set_concrete_field o key data in
        [ (o, None) ]
      else
        let lst =
          Expr_Hashtbl.fold o.symbolic_fields ~init:[]
            ~f:(fun ~key:k ~data:d acc ->
              if is_key_possible key k solver pc store then (k, d) :: acc else acc)
        in
        (* create an object for each possible equality *)
        let rets =
          List.map lst ~f:(fun (k, _) ->
              let o', pc' = create_object o key k store in
              set_concrete_field o' key data;
              Expr_Hashtbl.remove o'.symbolic_fields k;
              (o', pc'))
        in

        (*
         update current object with the condition of not
         being equal to any of the existing fields
        *)
        let new_pc = create_not_pct lst key store in
        if Encoding.Batch.check_sat solver (new_pc :: pc) then (
          let o' = clone o in
          Hashtbl.set o'.concrete_fields ~key:s ~data;
          (o', Some new_pc) :: rets)
        else rets
  | _ ->
      let temp =
        Hashtbl.length o.concrete_fields + Expr_Hashtbl.length o.symbolic_fields
      in
      if temp = 0 then (
        set_symbolic_field o key data;
        [ (o, None) ]
      )
      else
        let symbolic_conds =
          Expr_Hashtbl.fold o.symbolic_fields ~init:[]
            ~f:(fun ~key:k ~data:d acc ->
              if is_key_possible key k solver pc store then (k, d) :: acc else acc)
        in

        let concrete_conds =
          Hashtbl.fold o.concrete_fields ~init:[] ~f:(fun ~key:k ~data:d acc ->
              let k' = Expr.Val (Val.Str k) in
              if is_key_possible key k' solver pc store then (k', d) :: acc else acc)
        in

        (* Two maps because set functions differ *)
        let rets =
          List.map concrete_conds ~f:(fun (concrete_key, _) ->
              let o', pc' = create_object o key concrete_key store in
              set_concrete_field o' concrete_key data;
              (o', pc'))
        in
        let rets =
          rets
          @ List.map symbolic_conds ~f:(fun (symbolic_key, _) ->
                let o', pc' = create_object o key symbolic_key store in
                set_symbolic_field o' symbolic_key data;
                (o', pc'))
        in

        let new_pc = create_not_pct (concrete_conds @ symbolic_conds) key store in
        let check = Encoding.Batch.check_sat solver (new_pc :: pc)  in
        if check then(
          let o' = clone o in
          let _ = Expr_Hashtbl.set o'.symbolic_fields ~key ~data in
          (o', Some new_pc) :: rets
        )
        else rets

let get (o : 'a t) (key : vt) (solver : Encoding.Batch.t)
    (pc : encoded_pct list) (store : Sstore.t) : ('a t * encoded_pct option * 'a option) list =
  match key with
  | Expr.Val (Val.Str key_s) -> (
      let res = Hashtbl.find o.concrete_fields key_s in
      match res with
      | Some v -> 
        [ (o, None, Some v) ]        
      | None ->
          if Expr_Hashtbl.length o.symbolic_fields = 0 then [ (o, None, None) ]
          else
            let l =
              Expr_Hashtbl.fold o.symbolic_fields ~init:[]
                ~f:(fun ~key:k ~data:d acc ->
                  if is_key_possible key k solver pc store then (k, d) :: acc else acc)
            in

            let obj_list =
              List.map l ~f:(fun (k, v) ->
                  let o', pc' = create_object o key k store in
                  Expr_Hashtbl.remove o'.symbolic_fields k;
                  Hashtbl.set o'.concrete_fields ~key:key_s ~data:v;
                  (o', pc', Some v))
            in
            (* Does not match any symbolic value, create new pct *)
            let new_pc = create_not_pct l key store in
            if Encoding.Batch.check_sat solver (new_pc :: pc) then
              let o' = clone o in
              (o', Some new_pc, None) :: obj_list
            else obj_list)
  | _ -> (
      let res = get_symbolic_field o key in
      match res with
      | Some v -> 
        [ (o, None, Some v) ]
      | None ->
          let cond_list =
            Hashtbl.fold o.concrete_fields ~init:[]
              ~f:(fun ~key:k ~data:d acc ->
                let k' = Expr.Val (Val.Str k) in
                if is_key_possible key k' solver pc store then (k', d) :: acc else acc)
          in
          let cond_list =
            Expr_Hashtbl.fold o.symbolic_fields ~init:cond_list
              ~f:(fun ~key:k ~data:d acc ->
                if is_key_possible key k solver pc store then (k, d) :: acc else acc)
          in

          (* Get objects for all possible symb and concrete equalities *)
          let rets =
            List.map cond_list ~f:(fun (k, v) ->
                let o', pc' = create_object o key k store in
                (o', pc', Some v))
          in

          (* Does not match any symbolic value, create new pct *)
          let new_pc = create_not_pct cond_list key store in
          let rets = if Encoding.Batch.check_sat solver (new_pc :: pc) then (
            let o' = clone o in
            (o', Some new_pc, None) :: rets 
          ) else rets in 
          rets
      ) 

let delete (o : 'a t) (key : Expr.t) (solver : Encoding.Batch.t)
    (pc : encoded_pct list) (store : Sstore.t) : ('a t * encoded_pct option) list =
  match key with
  | Expr.Val (Val.Str s) ->
      if has_concrete_key o s then
        let _ = Hashtbl.remove o.concrete_fields s in
        [ (o, None) ]
      else if Expr_Hashtbl.length o.symbolic_fields = 0 then [ (o, None) ]
      else
        let lst =
          Expr_Hashtbl.fold o.symbolic_fields ~init:[]
            ~f:(fun ~key:k ~data:d acc ->
              if is_key_possible key k solver pc store then (k, d) :: acc else acc)
        in

        (* create an object for each possible equality *)
        let rets =
          List.map lst ~f:(fun (k, _) ->
              let o', pc' = create_object o key k store in
              Expr_Hashtbl.remove o'.symbolic_fields k;
              (o', pc'))
        in

        (*
            update current object with the condition of not
            being equal to any of the existing fields
        *)
        let new_pc = create_not_pct lst key store in
        if Encoding.Batch.check_sat solver (new_pc :: pc) then (
          let o' = clone o in
          (o', Some new_pc) :: rets)
        else rets
  | _ -> (
      let res = get_symbolic_field o key in
      match res with
      | Some v -> 
        Expr_Hashtbl.remove o.symbolic_fields key; 
        [ (o, None) ]
      | None ->
          let symbolic_list =
            Expr_Hashtbl.fold o.symbolic_fields ~init:[]
              ~f:(fun ~key:k ~data:d acc ->
                if is_key_possible key k solver pc store then (k, d) :: acc else acc)
          in

          let concrete_list =
            Hashtbl.fold o.concrete_fields ~init:[]
              ~f:(fun ~key:k ~data:d acc ->
                let k' = Expr.Val (Val.Str k) in
                if is_key_possible key k' solver pc store then (k', d) :: acc else acc)
          in

          (* Get objects for all possible symb equalities *)
          let rets =
            List.map symbolic_list ~f:(fun (k, _) ->
                let o', pc' = create_object o key k store in
                Expr_Hashtbl.remove o'.symbolic_fields k;
                (o', pc'))
          in
          (* Get objects for all possible concrete equalities *)
          let rets =
            List.map concrete_list ~f:(fun (k, _) ->
                let o', pc' = create_object o key k store in

                let s =
                  match k with
                  | Expr.Val (Val.Str s) -> s
                  | _ -> failwith "Invalid key value."
                in
                Hashtbl.remove o'.concrete_fields s;
                (o', pc'))
            @ rets
          in
          (* Does not match any symbolic value, create new pct *)
          let new_pc = create_not_pct (symbolic_list @ concrete_list) key store in
          if Encoding.Batch.check_sat solver (new_pc :: pc) then
            let o' = clone o in
            (o', Some new_pc) :: rets
          else rets)

(* let to_json (o : 'a t) (printer : 'a -> string) : string =
   let str_obj =
     Hashtbl.fold o ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
         (if String.(ac <> "{ ") then ac ^ ", " else ac)
         ^ Printf.sprintf "\"%s\": %s" n (printer v))
   in
   str_obj ^ " }" *)

let to_list (o : 'a t) : (String.t * 'a) list =
  (*TODO add symb values*)
  Hashtbl.to_alist o.concrete_fields

let get_fields (o : 'a t) : Expr.t list =
  let ret =
    List.map (Hashtbl.keys o.concrete_fields) ~f:(fun f -> Expr.Val (Val.Str f))
  in
  ret @ Expr_Hashtbl.keys o.symbolic_fields

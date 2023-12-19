open Core

module V = Sym_value.M

module type S = sig
  type encoded_pct = Encoding.Expression.t
  type obj
  type t
  type value

  val create : unit -> t
  val clone : t -> t
  val insert : t -> obj -> value
  val remove : t -> Loc.t -> unit
  val set : t -> Loc.t -> obj -> unit
  val get : t -> Loc.t -> obj option
  val has_field : t -> Loc.t -> value -> value

  val get_field :
    t ->
    Loc.t ->
    value ->
    Batch.t ->
    encoded_pct list ->
    (t * encoded_pct list * value option) list

  val set_field :
    t ->
    Loc.t ->
    value ->
    value ->
    Batch.t ->
    encoded_pct list ->
    (t * encoded_pct list) list

  val delete_field :
    t ->
    Loc.t ->
    value ->
    Batch.t ->
    encoded_pct list ->
    (t * encoded_pct list) list
end


module Object = struct
  type pct = V.value
  type encoded_pct = Encoding.Expression.t

  module ExprHash : Hashtbl.Key with type t = V.value = struct
    type t = V.value

    let hash (e : t) = Hashtbl.hash e
    let t_of_sexp _ = failwith "Not implemented."
    let sexp_of_t _ = failwith "Not implemented"
    let compare (e1 : t) (e2 : t) = Hashtbl.hash e1 - Hashtbl.hash e2
  end

  module Expr_Hashtbl = Hashtbl.Make (ExprHash)

  type t = {
    concrete_fields : (String.t, V.value) Hashtbl.t;
    symbolic_fields : V.value Expr_Hashtbl.t;
  }

  let create () : t =
    {
      concrete_fields = Hashtbl.create (module String);
      symbolic_fields = Hashtbl.create (module ExprHash);
    }

  let clone (o : t) : t =
    {
      concrete_fields = Hashtbl.copy o.concrete_fields;
      symbolic_fields = Expr_Hashtbl.copy o.symbolic_fields;
    }

  let to_string (o : t) (printer : V.value -> string) : string =
    let str_obj =
      Hashtbl.fold o.concrete_fields ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
          (if String.(ac <> "{ ") then ac ^ ", " else ac)
          ^ Printf.sprintf "\"%s\": %s" n (printer v))
      ^ "|||"
    in
    let str_obj =
      List.fold_left (Expr_Hashtbl.to_alist o.symbolic_fields)
        ~init:(str_obj ^ ", ") ~f:(fun acc (key, data) ->
          acc ^ Printf.sprintf "\"$symb_%s\": %s, " (V.Pp.pp key) (printer data))
    in
    str_obj ^ " }"

  let set_symbolic_field (o : t) (key : V.value) (data : V.value) : unit =
    Expr_Hashtbl.set o.symbolic_fields ~key ~data

  let set_concrete_field (o : t) (key : V.value) (data : V.value) : unit =
    match key with
    | V.Val (Val.Str s) -> Hashtbl.set o.concrete_fields ~key:s ~data
    | _ -> failwith ("bad key: " ^ V.Pp.pp key)

  let has_concrete_key (o : t) (key : string) : bool =
    let res = Hashtbl.find o.concrete_fields key in
    match res with Some _ -> true | None -> false

  let concrete_to_list (o : t) : (pct * V.value) list =
    let s_l = Hashtbl.to_alist o.concrete_fields in
    List.map s_l ~f:(fun (k, v) -> (V.Val (Val.Str k), v))

  let get_symbolic_field (o : t) (key : V.value) : V.value option =
    Expr_Hashtbl.find o.symbolic_fields key

  let mk_eq e1 e2 = V.BinOpt (Operator.Eq, e1, e2)

  let create_not_pct (l : (pct * V.value) list) (key : pct) : encoded_pct list =
    List.fold l ~init:[] ~f:(fun acc (pc, _) ->
        let ne = V.UnOpt (Operator.LogicalNot, mk_eq key pc) in
        let expr = Value_reducer.reduce ne |> Value_translator.translate in
        expr :: acc)

  let create_object (o : t) (k1 : pct) (k2 : pct) : t * encoded_pct list =
    let o' = clone o in
    let eq = Value_reducer.reduce (mk_eq k1 k2) |> Value_translator.translate in
    (o', [ eq ])

  let is_key_possible ?(b = false) (k1 : V.value) (k2 : V.value)
      (solver : Batch.t) (pc : encoded_pct list) : bool =
    let eq0 = mk_eq k1 k2 in
    let eq = Value_reducer.reduce eq0 |> Value_translator.translate in
    let ret = Batch.check solver (eq :: pc) in

    if b then (
      Printf.printf "\n\n";
      if not ret then
        List.iter pc ~f:(fun v ->
            Printf.printf "%s\n" (Encoding.Expression.to_string v));
      Printf.printf "create_object tested: %s, result: %b\n" (V.Pp.pp eq0) ret;
      Printf.printf "\n\n";
      ret)
    else ret

  let mk_ite e1 e2 e3 = V.TriOpt (Operator.ITE, e1, e2, e3)
  let is_val = function V.Val _ -> true | _ -> false

  let has_field (o : t) (k : V.value) : V.value =
    let open Val in
    assert (is_val k || V.is_symbolic k);
    if
      Hashtbl.is_empty o.concrete_fields
      && Expr_Hashtbl.is_empty o.symbolic_fields
    then V.Val (Bool false)
    else if is_val k then (
      match k with
      | V.Val (Str s) -> V.Val (Bool (Hashtbl.mem o.concrete_fields s))
      | _ ->
          Format.printf "has_field: %s@." (V.Pp.pp k);
          assert false)
    else
      let v0 =
        Expr_Hashtbl.fold o.symbolic_fields ~init:(V.Val (Bool false))
          ~f:(fun ~key ~data:_ accum ->
            mk_ite (mk_eq k key) (V.Val (Bool true)) accum)
      in
      Hashtbl.fold o.concrete_fields ~init:v0 ~f:(fun ~key ~data:_ accum ->
          mk_ite (mk_eq k (V.Val (Str key))) (V.Val (Bool true)) accum)

  let set (o : t) (key : V.value) (data : V.value) (solver : Batch.t)
      (pc : encoded_pct list) : (t * encoded_pct list) list =
    match key with
    | V.Val (Val.Str s) ->
        if has_concrete_key o s || Expr_Hashtbl.length o.symbolic_fields = 0
        then
          let _ = set_concrete_field o key data in
          [ (o, []) ]
        else
          let lst =
            Expr_Hashtbl.fold o.symbolic_fields ~init:[]
              ~f:(fun ~key:k ~data:d acc ->
                if is_key_possible key k solver pc then (k, d) :: acc else acc)
          in
          (* create an object for each possible equality *)
          let rets =
            List.map lst ~f:(fun (k, _) ->
                let o', pc' = create_object o key k in
                set_concrete_field o' key data;
                Expr_Hashtbl.remove o'.symbolic_fields k;
                (o', pc'))
          in

          (*
         update current object with the condition of not
         being equal to any of the existing fields
        *)
          let new_pc = create_not_pct lst key in
          if Batch.check solver (new_pc @ pc) then (
            let o' = clone o in
            Hashtbl.set o'.concrete_fields ~key:s ~data;
            (o', new_pc) :: rets)
          else rets
    | _ ->
        let temp =
          Hashtbl.length o.concrete_fields
          + Expr_Hashtbl.length o.symbolic_fields
        in
        if temp = 0 then (
          set_symbolic_field o key data;
          [ (o, []) ])
        else
          let symbolic_conds =
            Expr_Hashtbl.fold o.symbolic_fields ~init:[]
              ~f:(fun ~key:k ~data:d acc ->
                if is_key_possible key k solver pc then (k, d) :: acc else acc)
          in

          let concrete_conds =
            Hashtbl.fold o.concrete_fields ~init:[]
              ~f:(fun ~key:k ~data:d acc ->
                let k' = V.Val (Val.Str k) in
                if is_key_possible key k' solver pc then (k', d) :: acc else acc)
          in

          (* Two maps because set functions differ *)
          let rets =
            List.map concrete_conds ~f:(fun (concrete_key, _) ->
                let o', pc' = create_object o key concrete_key in
                set_concrete_field o' concrete_key data;
                (o', pc'))
          in
          let rets =
            rets
            @ List.map symbolic_conds ~f:(fun (symbolic_key, _) ->
                  let o', pc' = create_object o key symbolic_key in
                  set_symbolic_field o' symbolic_key data;
                  (o', pc'))
          in

          let new_pc = create_not_pct (concrete_conds @ symbolic_conds) key in
          let check = Batch.check solver (new_pc @ pc) in
          if check then
            let o' = clone o in
            let _ = Expr_Hashtbl.set o'.symbolic_fields ~key ~data in
            (o', new_pc) :: rets
          else rets

  let get (o : t) (key : V.value) (solver : Batch.t) (pc : encoded_pct list) :
      (t * encoded_pct list * V.value option) list =
    match key with
    | V.Val (Val.Str key_s) -> (
        let res = Hashtbl.find o.concrete_fields key_s in
        match res with
        | Some v -> [ (o, [], Some v) ]
        | None ->
            if Expr_Hashtbl.length o.symbolic_fields = 0 then [ (o, [], None) ]
            else
              let l =
                Expr_Hashtbl.fold o.symbolic_fields ~init:[]
                  ~f:(fun ~key:k ~data:d acc ->
                    if is_key_possible key k solver pc then (k, d) :: acc
                    else acc)
              in

              let obj_list =
                List.map l ~f:(fun (k, v) ->
                    let o', pc' = create_object o key k in
                    Expr_Hashtbl.remove o'.symbolic_fields k;
                    Hashtbl.set o'.concrete_fields ~key:key_s ~data:v;
                    (o', pc', Some v))
              in
              (* Does not match any symbolic value, create new pct *)
              let new_pc = create_not_pct l key in
              if Batch.check solver (new_pc @ pc) then
                let o' = clone o in
                (o', new_pc, None) :: obj_list
              else obj_list)
    | _ -> (
        let res = get_symbolic_field o key in
        match res with
        | Some v -> [ (o, [], Some v) ]
        | None ->
            let cond_list =
              Hashtbl.fold o.concrete_fields ~init:[]
                ~f:(fun ~key:k ~data:d acc ->
                  let k' = V.Val (Val.Str k) in
                  if is_key_possible key k' solver pc then (k', d) :: acc
                  else acc)
            in
            let cond_list =
              Expr_Hashtbl.fold o.symbolic_fields ~init:cond_list
                ~f:(fun ~key:k ~data:d acc ->
                  if is_key_possible key k solver pc then (k, d) :: acc else acc)
            in

            (* Get objects for all possible symb and concrete equalities *)
            let rets =
              List.map cond_list ~f:(fun (k, v) ->
                  let o', pc' = create_object o key k in
                  (o', pc', Some v))
            in

            (* Does not match any symbolic value, create new pct *)
            let new_pc = create_not_pct cond_list key in
            let rets =
              if Batch.check solver (new_pc @ pc) then
                let o' = clone o in
                (o', new_pc, None) :: rets
              else rets
            in
            rets)

  let delete (o : t) (key : V.value) (solver : Batch.t) (pc : encoded_pct list)
      : (t * encoded_pct list) list =
    match key with
    | V.Val (Val.Str s) ->
        if has_concrete_key o s then
          let _ = Hashtbl.remove o.concrete_fields s in
          [ (o, []) ]
        else if Expr_Hashtbl.length o.symbolic_fields = 0 then [ (o, []) ]
        else
          let lst =
            Expr_Hashtbl.fold o.symbolic_fields ~init:[]
              ~f:(fun ~key:k ~data:d acc ->
                if is_key_possible key k solver pc then (k, d) :: acc else acc)
          in

          (* create an object for each possible equality *)
          let rets =
            List.map lst ~f:(fun (k, _) ->
                let o', pc' = create_object o key k in
                Expr_Hashtbl.remove o'.symbolic_fields k;
                (o', pc'))
          in

          (*
            update current object with the condition of not
            being equal to any of the existing fields
        *)
          let new_pc = create_not_pct lst key in
          if Batch.check solver (new_pc @ pc) then
            let o' = clone o in
            (o', new_pc) :: rets
          else rets
    | _ -> (
        let res = get_symbolic_field o key in
        match res with
        | Some _ ->
            Expr_Hashtbl.remove o.symbolic_fields key;
            [ (o, []) ]
        | None ->
            let symbolic_list =
              Expr_Hashtbl.fold o.symbolic_fields ~init:[]
                ~f:(fun ~key:k ~data:d acc ->
                  if is_key_possible key k solver pc then (k, d) :: acc else acc)
            in

            let concrete_list =
              Hashtbl.fold o.concrete_fields ~init:[]
                ~f:(fun ~key:k ~data:d acc ->
                  let k' = V.Val (Val.Str k) in
                  if is_key_possible key k' solver pc then (k', d) :: acc
                  else acc)
            in

            (* Get objects for all possible symb equalities *)
            let rets =
              List.map symbolic_list ~f:(fun (k, _) ->
                  let o', pc' = create_object o key k in
                  Expr_Hashtbl.remove o'.symbolic_fields k;
                  (o', pc'))
            in
            (* Get objects for all possible concrete equalities *)
            let rets =
              List.map concrete_list ~f:(fun (k, _) ->
                  let o', pc' = create_object o key k in

                  let s =
                    match k with
                    | V.Val (Val.Str s) -> s
                    | _ -> failwith "Invalid key value."
                  in
                  Hashtbl.remove o'.concrete_fields s;
                  (o', pc'))
              @ rets
            in
            (* Does not match any symbolic value, create new pct *)
            let new_pc = create_not_pct (symbolic_list @ concrete_list) key in
            if Batch.check solver (new_pc @ pc) then
              let o' = clone o in
              (o', new_pc) :: rets
            else rets)

  (* let to_json (o : 'a t) (printer : 'a -> string) : string =
     let str_obj =
       Hashtbl.fold o ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
           (if String.(ac <> "{ ") then ac ^ ", " else ac)
           ^ Printf.sprintf "\"%s\": %s" n (printer v))
     in
     str_obj ^ " }" *)

  let to_list (o : t) : (V.value * 'a) list =
    (*TODO add symb values*)
    concrete_to_list o @ Expr_Hashtbl.to_alist o.symbolic_fields

  let get_fields (o : t) : V.value list =
    let ret =
      List.map (Hashtbl.keys o.concrete_fields) ~f:(fun f -> V.Val (Val.Str f))
    in
    ret @ Expr_Hashtbl.keys o.symbolic_fields
end

module Heap = struct
  type encoded_pct = Encoding.Expression.t
  type obj = Object.t
  type value = V.value
  type t = { parent : t option; map : (Loc.t, obj) Hashtbl.t }

  let create () : t = { parent = None; map = Hashtbl.create (module String) }

  let clone (h : t) : t =
    { parent = Some h; map = Hashtbl.create (module String) }

  let insert (h : t) (obj : obj) : value =
    let loc = Loc.newloc () in
    Hashtbl.set h.map ~key:loc ~data:obj;
    V.Val (Val.Loc loc)

  let remove (h : t) (l : Loc.t) : unit = Hashtbl.remove h.map l

  let set (h : t) (key : Loc.t) (data : obj) : unit =
    Hashtbl.set h.map ~key ~data

  let rec get (h : t) (l : Loc.t) : obj option =
    let result = Hashtbl.find h.map l in
    match result with
    | Some _o -> result
    | None -> (
        let obj = Option.bind h.parent ~f:(fun h -> get h l) in
        match obj with
        | Some o ->
            let o' = Object.clone o in
            set h l o';
            Some o'
        | None -> None)

  let has_field (h : t) (l : Loc.t) (f : value) : value =
    Option.value_map (get h l) ~default:(V.Val (Val.Bool false)) ~f:(fun o ->
        Object.has_field o f)

  let get_field (heap : t) (loc : Loc.t) (field : value) (solver : Batch.t)
      (pc : encoded_pct list) : (t * encoded_pct list * value option) list =
    let obj = get heap loc in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.get o field solver pc))
    in
    match res with
    | None -> failwith ("get Return is never none. loc: " ^ loc ^ V.Pp.pp field)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc, v) ] ->
            set heap loc obj;
            [ (heap, pc, v) ]
        | _ ->
            List.map objs ~f:(fun (obj, pc, v) ->
                let heap' = clone heap in
                set heap' loc obj;
                (heap', pc, v)))

  let set_field (heap : t) (loc : Loc.t) (field : value) (v : value)
      (solver : Batch.t) (pc : encoded_pct list) : (t * encoded_pct list) list =
    let obj = get heap loc in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.set o field v solver pc))
    in
    match res with
    | None -> failwith ("set Return is never none. loc: " ^ loc)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc) ] ->
            set heap loc obj;
            [ (heap, pc) ]
        | _ ->
            List.map objs ~f:(fun (obj, pc) ->
                let heap' = clone heap in
                set heap' loc obj;
                (heap', pc)))

  let delete_field (heap : t) (loc : Loc.t) (field : value) (solver : Batch.t)
      (pc : encoded_pct list) : (t * encoded_pct list) list =
    let obj = get heap loc in
    let res =
      Option.bind obj ~f:(fun o -> Some (Object.delete o field solver pc))
    in

    match res with
    | None -> failwith ("delete Return is never none. loc: " ^ loc)
    | Some objs -> (
        (* Don't clone heap unless necessary *)
        match objs with
        | [ (obj, pc') ] ->
            set heap loc obj;
            [ (heap, pc') ]
        | _ ->
            List.map objs ~f:(fun (obj, pc) ->
                let heap' = clone heap in
                set heap' loc obj;
                (heap', pc)))

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
end

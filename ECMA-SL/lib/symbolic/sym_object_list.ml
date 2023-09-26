open Core

module Value = Sym_value.M
module Reducer = Value_reducer
module Translator = Value_translator

module M : sig
  type value = Value.value
  type encoded_pct = Encoding.Expression.t
  type t

  val create : unit -> t
  val clone : t -> t
  val create_clone : t -> value -> value -> t * encoded_pct list
  val to_list : t -> (value * value) list
  val get_fields : t -> value list
  val has_field : t -> value -> Batch.t -> encoded_pct list -> value
  val set : t -> key:value -> data:value -> t
  val get : t -> value -> Batch.t -> encoded_pct list -> value
  val delete : t -> value -> t
  val to_string : t -> string
end = struct

type value = Value.value
type pct = Value.value
type encoded_pct = Encoding.Expression.t

let rec_size = 1000

(* module ExprHash = struct *)
(*   type t = Value.value *)

(*   let hash (e : t) = Hashtbl.hash e *)
(*   let t_of_sexp _e = assert false *)
(*   let sexp_of_t _e = assert false *)
(*   let compare (e1 : t) (e2 : t) = compare (Hashtbl.hash e1)  (Hashtbl.hash e2) *)
(* end *)
(* module Expr_Hashtbl = Hashtbl.Make (ExprHash) *)

type obj_record = {
  concrete_fields : (string, value option) Hashtbl.t;
  symbolic_field : (value * value option) option;
}

type t = obj_record list

let create_obj_record () : obj_record =
  { concrete_fields = Hashtbl.create (module String); symbolic_field = None }

let create () : t = [ create_obj_record () ]

let clone (o : t) : t =
  match o with
  | h :: tl ->
      let h' =
        {
          concrete_fields = Hashtbl.copy h.concrete_fields;
          symbolic_field = h.symbolic_field;
        }
      in

      h' :: tl
  | _ -> assert false

let obj_record_to_string (o_r : obj_record)  : string =
  let aux e = match e with Some v -> Value.Pp.pp v | None -> "deleted" in

  let str_obj =
    Hashtbl.fold o_r.concrete_fields ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
        (if String.(ac <> "{ ") then ac ^ ", " else ac)
        ^ Printf.sprintf "\"%s\": %s" n (aux v))
    ^ "|"
  in
  match o_r.symbolic_field with
  | None -> str_obj ^ " }"
  | Some (key, data) ->
      str_obj ^ Printf.sprintf "\"%s\": %s" (Value.Pp.pp key) (aux data) ^ " }"

let to_string (o : t) : string =
  List.fold o ~init:"" ~f:(fun acc o_r -> acc ^ obj_record_to_string o_r ^ "@")

(* let record_has_concrete_key (o : obj_record) (key : string) : bool = *)
(*   let res = Hashtbl.find o.concrete_fields key in *)
(*   match res with Some _ -> true | None -> false *)

(* let record_concrete_list (o : obj_record) : (value * value option) list = *)
(*   let s_l = Hashtbl.to_alist o.concrete_fields in *)
(*   List.map s_l ~f:(fun (k, v) -> (Value.Val (Val.Str k), v)) *)

let record_concrete_list2 (o : obj_record) : (value * value) list =
  let s_l = Hashtbl.to_alist o.concrete_fields in
  List.fold s_l ~init:[] ~f:(fun acc (k, v) ->
      match v with
      | Some v' -> acc @ [ (Value.Val (Val.Str k), v') ]
      | None -> acc)

let record_concrete_keys (o : obj_record) : value list =
  let s_l = Hashtbl.keys o.concrete_fields in
  List.map s_l ~f:(fun k -> Value.Val (Val.Str k))

(* let concrete_to_list (o : t) : (value * value option) list = *)
(*   List.fold o ~init:[] ~f:(fun accum o_r -> accum @ record_concrete_list o_r) *)

let mk_eq e1 e2 = Value.BinOpt (Operators.Eq, e1, e2)
let mk_ite e1 e2 e3 = Value.TriOpt (Operators.ITE, e1, e2, e3)
let mk_or e1 e2 = Value.BinOpt (Operators.Log_Or, e1, e2)
let mk_not e1 = Value.UnOpt (Operators.Not, e1)

let is_key_possible (k1 : value) (k2 : value) (solver : Batch.t)
    (pc : encoded_pct list) : bool =
  let eq0 = mk_eq k1 k2 in
  let eq = Reducer.reduce eq0 |> Translator.translate in
  Batch.check solver (eq :: pc)

(* let create_not_pct (l : (pct * value) list) (key : pct) : encoded_pct list = *)
(*   List.fold l ~init:[] ~f:(fun acc (pc, _) -> *)
(*       let ne = Value.UnOpt (Operators.Not, mk_eq key pc) in *)
(*       let expr = Reducer.reduce ne |> Translator.translate in *)
(*       expr :: acc) *)

let create_clone (o : t) (k1 : pct) (k2 : pct) : t * encoded_pct list =
  let o' = clone o in
  let eq = Reducer.reduce (mk_eq k1 k2) |> Translator.translate in
  (o', [ eq ])

(* let create_ite (lst : (pct * pct) list) (key : value) (pc : encoded_pct list) *)
(*     (solver : Batch.t) : value = *)
(*   let undef = Value.Val (Val.Symbol "undefined") in *)
(*   let false_e = Value.Val (Val.Bool true) in *)
(*   let ite, new_pc = *)
(*     List.fold lst ~init:(undef, false_e) ~f:(fun (acc_val, acc_pc) (k, d) -> *)
(*         let eq = Reducer.reduce (mk_eq key k) in *)
(*         let acc_val = Reducer.reduce (mk_ite eq d acc_val) in *)
(*         let p = if Value.equal false_e acc_pc then eq else mk_or acc_pc eq in *)
(*         (acc_val, p)) *)
(*   in *)

(*   let not_new = mk_not new_pc in *)
(*   let not_new = Translator.translate not_new in *)

(*   if Batch.check solver (not_new :: pc) then ite *)
(*   else *)
(*     let _, tv = List.hd_exn lst in *)
(*     List.fold (List.tl_exn lst) ~init:tv ~f:(fun acc_val (k, d) -> *)
(*         let eq = Reducer.reduce (mk_eq key k) in *)
(*         let acc_val = Reducer.reduce (mk_ite eq d acc_val) in *)
(*         acc_val) *)

let mk_ite_expr (conds : (value * value) list list) (default_val : value)
    : value =
  List.fold ~init:default_val
    ~f:(fun acc_ite l ->
      List.fold l ~init:acc_ite ~f:(fun acc (cond, data) ->
          let eq = Reducer.reduce cond in
          let ite = Reducer.reduce (mk_ite eq data acc) in
          ite))
    conds

let get_prop_rec (o_rec : obj_record) (prop : value)
    (get_val : value option -> value) (solver : Batch.t)
    (pc : encoded_pct list) :
    (value * value) list * value option option =
  let open Value in
  let open Val in
  let ret_lst, ret_e =
    match o_rec.symbolic_field with
    | None -> ([], None)
    | Some (prop', v) when Value.equal prop' prop -> ([], Some v)
    | Some (prop', v) ->
        let eq = Reducer.reduce (mk_eq prop' prop) in
        let new_pc = Translator.translate eq in
        if is_key_possible prop' prop solver (new_pc :: pc) then
          ([ (eq, get_val v) ], None)
        else ([], None)
  in
  let ret_lst, ret_e =
    match prop with
    | Val (Str p) when Hashtbl.mem o_rec.concrete_fields p ->
        (ret_lst, Some (Hashtbl.find_exn o_rec.concrete_fields p))
    | Val (Str _p) -> (ret_lst, ret_e)
    | _ ->

      match ret_e with
      | Some _e -> ret_lst, ret_e
      | None ->
        ( Hashtbl.fold o_rec.concrete_fields ~init:ret_lst
            ~f:(fun ~key:k ~data:v acc ->
              if is_key_possible (Val (Str k)) prop solver pc then
                let eq = Reducer.reduce (mk_eq (Val (Str k)) prop) in
                let ret = (eq, get_val v) in
                ret :: acc
              else acc),
          ret_e )
  in
  (ret_lst, ret_e)


let rec get_prop_aux ?(default_val = Value.Val (Val.Bool false)) (o : t)
    (prop : value) (get_val : value option -> value)
    (lst : (value * value) list list) (solver : Batch.t)
    (pc : encoded_pct list) :
    (value * value) list list * value =
  match o with
  | [] -> (lst, default_val)
  | o_rec :: o_rest -> (
      let lst', final_val = get_prop_rec o_rec prop get_val solver pc in
      match (lst', final_val) with
      | [], Some e -> (lst, get_val e)
      | lst', Some e -> (lst' :: lst, get_val e)
      | [], None ->
          get_prop_aux ~default_val o_rest prop get_val lst solver pc
      | ((_, v) :: rest as lst'), None ->
          let false_e = Value.Val (Val.Bool false) in
          let new_pc =
            List.fold lst' ~init:false_e ~f:(fun acc (eq, _) ->
                if Value.equal false_e acc then eq else mk_or acc eq)
          in
          let not_new = mk_not new_pc in
          let not_new = Translator.translate not_new in

          if Batch.check solver (not_new :: pc) then
            get_prop_aux ~default_val o_rest prop get_val (lst' :: lst) solver
              (not_new :: pc)

          (* Este caso existe? *)
          (* else if List.length lst + List.length lst' = 0 then ([], default_val) *)
          else (rest :: lst, v))

let get_prop ?(default_val = Value.Val (Val.Bool false)) (o : t) (prop : value)
    (solver : Batch.t) (pc : encoded_pct list)
    (get_val : value option -> value) : value =
  let conds, last_val =
    get_prop_aux ~default_val o prop get_val [] solver pc
  in
  mk_ite_expr conds last_val

let has_field (obj : t) (k : value) (solver : Batch.t)
    (pc : encoded_pct list) : value =
  get_prop obj k solver pc (fun v -> Value.Val (Val.Bool (Option.is_some v)))

let set (o : t) ~(key : value) ~(data : value) : t =
  match key with
  | Value.Val (Val.Str key_s) ->
      let o_r = List.hd_exn o in
      if Hashtbl.length o_r.concrete_fields >= rec_size then
        let new_or = create_obj_record() in
        Hashtbl.set new_or.concrete_fields ~key:key_s ~data:(Some data);
        new_or :: o
      else
        let _ = Hashtbl.set o_r.concrete_fields ~key:key_s ~data:(Some data) in
        o
  | _ ->
      let o_r = List.hd_exn o in
      let tl = List.tl_exn o in
      let new_written = { o_r with symbolic_field = Some (key, Some data) } in
      let new_rec = create_obj_record () in
      new_rec :: new_written :: tl

let get (o : t) (key : value) (solver : Batch.t) (pc : encoded_pct list)
    : value =
  let undef = Value.Val (Val.Symbol "undefined") in
  let get_val v = match v with Some v -> v | _ -> undef in
  get_prop ~default_val:undef o key solver pc get_val

let delete (o : t) (key : value) : t =
  match key with
  | Value.Val (Val.Str key_s) ->
      let o_r = List.hd_exn o in
      Hashtbl.set o_r.concrete_fields ~key:key_s ~data:None;
      o
  | _ ->
      let o_r = List.hd_exn o in
      let tl = List.tl_exn o in
      let new_written = { o_r with symbolic_field = Some (key, None) } in
      let new_rec = create_obj_record () in
      new_rec :: new_written :: tl

(* let to_json (o : 'a t) (printer : 'a -> string) : string = *)
(*    let str_obj = *)
(*      Hashtbl.fold o ~init:"{ " ~f:(fun ~key:n ~data:v ac -> *)
(*          (if String.(ac <> "{ ") then ac ^ ", " else ac) *)
(*          ^ Printf.sprintf "\"%s\": %s" n (printer v)) *)
(*    in *)
(*    str_obj ^ " }" *)

let to_list (o : t) : (value * value) list =
  let c, s =
    List.fold o ~init:([], []) ~f:(fun (concrete, symb) o_r ->
        let s =
          match o_r.symbolic_field with
          | None -> symb
          | Some (k, Some v') -> (k, v') :: symb
          | _ -> symb
        in
        (concrete @ record_concrete_list2 o_r, s))
  in
  c @ s

let get_fields (o : t) : value list =
  let c, s =
    List.fold o ~init:([], []) ~f:(fun (concrete, symb) o_r ->
        let s =
          match o_r.symbolic_field with
          | None -> symb
          | Some (key, _data) -> key :: symb
        in
        (concrete @ record_concrete_keys o_r, s))
  in
  c @ s
end

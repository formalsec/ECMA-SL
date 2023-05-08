open Core

type vt = Expr.t
type pct = Expr.t

type 'a t = {
  concrete_fields : (String.t, 'a) Hashtbl.t;
  mutable symbolic_fields : (vt * 'a) list;
}

let create () : 'a t =
  { concrete_fields = Hashtbl.create (module String); symbolic_fields = [] }

let clone (o : 'a t) : 'a t =
  {
    concrete_fields = Hashtbl.copy o.concrete_fields;
    symbolic_fields =
      List.map o.symbolic_fields ~f:(fun (field, v) : (Expr.t * Expr.t) ->
          (field, v));
  }

let set_symbolic_list (o : 'a t) (key : vt) (data : 'a) : unit =
  (* let rec replace_entry (seen : (vt * 'a) list) (list : (vt * 'a) list) ((target_key, new_data) : (vt * 'a)) =
       match list with
       | [] -> {o with symbolic_fields = [(key, data)] @ seen}
       | (key, v) :: t ->
         if (Expr.equal target_key key) then
           {o with symbolic_fields = seen @ [(target_key, new_data)] @ t }
         else
           replace_entry ((key, v)::seen) t (target_key, new_data)
     in *)
  let rec replace_entry (seen : (vt * 'a) list) (list : (vt * 'a) list)
      ((target_key, new_data) : vt * 'a) =
    match list with
    | [] -> [ (key, data) ] @ seen
    | (key, v) :: t ->
        if Expr.equal target_key key then seen @ [ (target_key, new_data) ] @ t
        else replace_entry ((key, v) :: seen) t (target_key, new_data)
  in
  let new_l = replace_entry [] o.symbolic_fields (key, data) in
  o.symbolic_fields <- new_l

let set_concrete_field (o : 'a t) (key : vt) (data : 'a) : unit =
  match key with
  | Expr.Val (Val.Str s) -> Hashtbl.set o.concrete_fields ~key:s ~data
  | _ -> failwith ("bad key: " ^ (Expr.str key))

let has_concrete_key (o : 'a t) (key : string) : bool =
  let res = Hashtbl.find o.concrete_fields key in
  match res with Some _ -> true | None -> false

let concrete_to_list (o : 'a t) : (pct * 'a) list =
  let s_l = Hashtbl.to_alist o.concrete_fields in
  List.map s_l ~f:(fun (k, v) -> (Expr.Val (Val.Str k), v))

let set (o : 'a t) (key : vt) (data : 'a) : ('a t * pct) list =
  (* Hashtbl.set o.concrete_fields ~key ~data *)
  let create_branching (field_list : (pct * 'a) list) (key : vt) (v : vt)
      (curr_key : vt) (set_fun : 'a t -> vt -> 'a -> unit) 
      (remove_symb_field : bool) : 'a t * pct =
    let true_pct = Expr.BinOpt (Operators.Eq, key, curr_key) in
    (* check if pct are true, only add those *)
    let true_obj = clone o in
    (* possible problem copying list *)
    if remove_symb_field then 
      let _ = true_obj.symbolic_fields <- field_list in
      let _ = set_fun true_obj key v in
      (true_obj, true_pct)
    else
      let _ = set_fun true_obj curr_key v in
      (true_obj, true_pct)
  in

  let rec create_not_branch (acc : pct) (o : 'a t) (key : vt) (v : vt) (new_objects : ('a t * pct) list) 
    : ('a t * pct) = 

    match new_objects with 
    | [] -> (
      let o' = clone o in
      let _ =
        match key with
        | Expr.Symbolic (_, _) -> set_symbolic_list o' key v;
        | Expr.Val (Val.Str s) -> set_concrete_field o' key v;
        | _ -> failwith "invalid key"
        in
      (o', acc))
    | (_, pc) :: t -> 
      let ne = Expr.UnOpt (Operators.Not, pc) in
      create_not_branch (Expr.BinOpt(Operators.Log_And, ne, acc)) o key v t
  in
    
  let rec create_branch_objects (key : vt) (new_val : vt)
      (seen : (pct * 'a) list) (val_list : (pct * 'a) list)
      (acc : ('a t * pct) list) (set_fun : 'a t -> vt -> 'a -> unit) 
      (create_not : bool) (remove_symb_field : bool) : ('a t * pct) list =
    match val_list with
    | [] -> 
      if create_not then 
        (create_not_branch (Expr.Val (Val.Bool true)) o key data acc) :: acc
      else
        acc 
    | (curr_key, v) :: t ->
        let bs = create_branching (seen @ t) key new_val curr_key set_fun remove_symb_field in
        create_branch_objects key new_val ((curr_key, v) :: seen) t (bs :: acc)
          set_fun create_not remove_symb_field
  in

  match key with
  | Expr.Symbolic (tp, v) -> (
      if (Hashtbl.length o.concrete_fields + List.length o.symbolic_fields) = 0 then
        (
          set_symbolic_list o key data;
          [ (o, Expr.Val (Val.Bool true)) ]
        )
      else
        let symb_objs = create_branch_objects key data [] o.symbolic_fields [] set_symbolic_list false false in
        let concrete_objs = create_branch_objects key data [] (concrete_to_list o) symb_objs set_concrete_field true false in
        concrete_objs
  )
  | Expr.Val (Val.Str s) ->
      (* has_concrete_key poupa ida ao solver? confirmar *)
      if has_concrete_key o s || List.length o.symbolic_fields = 0 then
        let _ = set_concrete_field o key data in
        [ (o, Expr.Val (Val.Bool true)) ]
      else
        create_branch_objects key data [] o.symbolic_fields []
          set_concrete_field true true
  | _ -> failwith "invalid key type for object"

(* in replace_entry [] o.symbolic_fields (key, data)  *)

let get (o : 'a t) (key : String.t) : 'a option =
  Hashtbl.find o.concrete_fields key

let delete (o : 'a t) (f : String.t) : unit = Hashtbl.remove o.concrete_fields f

let to_string (o : 'a t) (printer : 'a -> string) : string =
  let str_obj =
    Hashtbl.fold o.concrete_fields ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
        (if String.(ac <> "{ ") then ac ^ ", " else ac)
        ^ Printf.sprintf "\"%s\": %s" n (printer v))
  in
  let str_obj =
    List.fold_left o.symbolic_fields ~init:(str_obj ^ ", ")
      ~f:(fun acc (key, data) ->
        acc ^ Printf.sprintf "\"$symb_%s\": %s, " (Expr.str key) (printer data))
  in
  str_obj ^ " }"

(* let to_json (o : 'a t) (printer : 'a -> string) : string =
   let str_obj =
     Hashtbl.fold o ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
         (if String.(ac <> "{ ") then ac ^ ", " else ac)
         ^ Printf.sprintf "\"%s\": %s" n (printer v))
   in
   str_obj ^ " }" *)

let to_list (o : 'a t) : (String.t * 'a) list =
  Hashtbl.to_alist o.concrete_fields

let get_fields (o : 'a t) : String.t list = Hashtbl.keys o.concrete_fields

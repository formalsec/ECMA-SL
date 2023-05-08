let rec print_os (os : (Expr.t S_object.t * Expr.t) list) (curr : int) : int =
  match os with
  | [] ->
      Printf.printf "\n";
      curr
  | (o, pct) :: t ->
      let s1 = S_object.to_string o (fun v -> Expr.str v) in
      let s2 = Expr.str pct in
      let _ = Printf.printf "Object%d: %s\nPCT: %s\n" curr s1 s2 in
      print_os t (curr + 1)
in
let o : Expr.t S_object.t = S_object.create () in
let objects =
  S_object.set o (Expr.Val (Val.Str "key")) (Expr.Val (Val.Str "val"))
in
Printf.printf "Object: %s\n" (S_object.to_string o (fun v -> Expr.str v));
let count = print_os objects 0 in
let o, _ =
  match count with 1 -> List.hd objects | _ -> failwith "no objects"
in

let objects2 =
  S_object.set o
    (Expr.Symbolic (Type.StrType, Expr.Var "symb_k"))
    (Expr.Val (Val.Str "new_symb_val"))
in
let count2 = print_os objects2 0 in
let o2, _ =
  match count2 with 1 -> List.hd objects2 | _ -> failwith "no objects"
in
let objects2 =
  S_object.set o2
    (Expr.Symbolic (Type.StrType, Expr.Var "symb_k2"))
    (Expr.Val (Val.Str "new_symb_val2"))
in
let count2 = print_os objects2 0 in
let o2, _ =
  match count2 with 1 -> List.hd objects2 | _ -> failwith "no objects"
in
let objects3 =
  S_object.set o2 (Expr.Val (Val.Str "key2")) (Expr.Val (Val.Str "val2"))
in
let _ = print_os objects3 0 in

Printf.printf "done."

(* let o2 = S_object.clone o in
   let o2 = S_object.set o2 (Expr.Val (Val.Str "key")) (Expr.Val (Val.Str "new_val")) in
   let o2 = S_object.set o2 (Expr.Val (Val.Str "key2")) (Expr.Val (Val.Str "new_val2")) in
   Printf.printf "Object1: %s\n" (S_object.to_string o (fun v -> Expr.str v));
   Printf.printf "Object2: %s\n" (S_object.to_string o2 (fun v -> Expr.str v)); *)

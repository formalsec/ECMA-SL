type 'sl t = (string, 'sl) Hashtbl.t

let create (var_vals : (string * 'sl) list) : 'sl t =
  List.to_seq var_vals |> Hashtbl.of_seq

let get (store : 'sl t) (var : string) : 'sl = Hashtbl.find store var

let get_opt (store : 'sl t) (var : string) : 'sl option =
  Hashtbl.find_opt store var

let set (store : 'sl t) (var : string) (v : 'sl) : unit =
  Hashtbl.replace store var v

let str (sl_printer : 'sl -> string) (store : 'sl t) : string =
  let binding_str x v = Printf.sprintf "(%s, %s)" x (sl_printer v) in
  let store_str_f x v acc = binding_str x v :: acc in
  Hashtbl.fold store_str_f store [] |> String.concat ", "

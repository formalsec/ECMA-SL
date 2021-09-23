
let float64_to_le_bytes (x : float) : int64 list = 
    let v = Int64.bits_of_float x in
    let lst = ref [] in 
    for i = 0 to 7 do
       (* lst := (Int64.logand 255L (Int64.shift_right v (i * 8))) :: !lst; *)
       lst := !lst @ [ (Int64.logand 255L (Int64.shift_right v (i * 8))) ];
    done;
    !lst 


(*
let float64_to_le_bytes (v : Val.t) : Val.t = match v with
  | Flt x -> 
    let v = Int64.bits_of_float x in
    let lst = ref [] in 
    for i = 0 to 7 do
       lst := (Int64.logand 255L (Int64.shift_right v (i * 8))) :: !lst;
    done
    Flt (x)
  | _      -> invalid_arg "Exception in Oper.float64_to_le_bytes: this operation is only applicable to Float arguments"

let float64_to_be_bytes (v : Val.t) : Val.t = match v with
  | Flt x -> 
    let v = Int64.bits_of_float x in
    for i = 0 to 7 do
      Printf.printf "%Lx " (Int64.logand 255L (Int64.shift_right v ((7 - i) * 8))) ;
    done
  | _      -> invalid_arg "Exception in Oper.float64_to_be_bytes: this operation is only applicable to Float arguments"

let float32_to_le_bytes (v : Val.t) : Val.t = match v with
  | Flt x -> 
    let v = Int32.bits_of_float x in
    for i = 0 to 7 do
      Printf.printf "%Lx " (Int64.logand 255l (Int64.shift_right v (i * 8))) ;
    done
  | _      -> invalid_arg "Exception in Oper.float32_to_le_bytes: this operation is only applicable to Float arguments"

let float32_to_be_bytes (v : Val.t) : Val.t = match v with
  | Flt x -> 
    let v = Int32.bits_of_float x in
    for i = 0 to 7 do
      Printf.printf "%Lx " (Int64.logand 255l (Int64.shift_right v ((7 - i) * 8))) ;
    done
  | _      -> invalid_arg "Exception in Oper.float32_to_be_bytes: this operation is only applicable to Float arguments"

  *)
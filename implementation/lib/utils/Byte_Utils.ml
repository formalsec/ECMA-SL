let float64_to_le_bytes (x : float) : int64 list = 
    let v = Int64.bits_of_float x in
    let lst = ref [] in 
    for i = 0 to 7 do
       lst := !lst @ [ (Int64.logand 255L (Int64.shift_right v (i * 8))) ];
    done;
    !lst

let float64_to_be_bytes (x : float) : int64 list = 
    let v = Int64.bits_of_float x in
    let lst = ref [] in 
    for i = 0 to 7 do
       lst := (Int64.logand 255L (Int64.shift_right v (i * 8))) :: !lst;
    done;
    !lst

let float32_to_le_bytes (x : float) : int32 list = 
    let v = Int32.bits_of_float x in
    let lst = ref [] in 
    for i = 0 to 3 do
       lst := !lst @ [ (Int32.logand 255l (Int32.shift_right v (i * 8))) ];
    done;
    !lst

let float32_to_be_bytes (x : float) : int32 list = 
    let v = Int32.bits_of_float x in
    let lst = ref [] in 
    for i = 0 to 3 do
       lst := (Int32.logand 255l (Int32.shift_right v (i * 8))) :: !lst;
    done;
    !lst

let float64_from_le_bytes (bytes : int64 list) : float =
  let res = ref 0L in
  for i = 0 to 7 do
     res := Int64.add !res (Int64.shift_left (List.nth bytes i) (i * 8))
  done;
  Int64.float_of_bits !res

let float64_from_be_bytes (bytes : int64 list) : float =
   let res = ref 0L in
   for i = 0 to 7 do
     res := Int64.add !res (Int64.shift_left (List.nth bytes i) ((7 - i) * 8))
   done;
   Int64.float_of_bits !res

let float32_from_le_bytes (bytes : int32 list) : float =
  let res = ref 0l in
  for i = 0 to 3 do
     res := Int32.add !res (Int32.shift_left (List.nth bytes i) (i * 8))
  done;
  Int32.float_of_bits !res

let float32_from_be_bytes (bytes : int32 list) : float =
   let res = ref 0l in
   for i = 0 to 3 do
     res := Int32.add !res (Int32.shift_left (List.nth bytes i) ((3 - i) * 8))
   done;
   Int32.float_of_bits !res
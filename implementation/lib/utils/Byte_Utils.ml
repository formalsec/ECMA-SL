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

let int_to_be_bytes (x, n : float * int) : int list = 
   Printf.printf "debug 42: int_to_be_bytes x: %f, n: %d\n" x n;
   
   let rec loop (lst : int list) (value : int) (j : int) =
      Printf.printf "debug 42: int_to_be_bytes value: %d, j: %d\n" value j; 
      if (j < n) then  
         loop ([Int.logand value 255] @ lst) (Int.shift_right value 8) (j+1)
      else lst in 
      loop [] (Float.to_int x) 0
         
let uint_from_le_bytes (arr, n : int array * int) : float =
   Printf.printf "debug 42: uint_from_le_bytes n: %d\n" n;
   let len = Array.length arr in
   let rec loop (value : int ) (j : int) =
      Printf.printf "debug 42: uint_from_le_bytes value: %d, j: %d\n" value j; 
      if (j >= 0) then  
         loop (value*256 + Array.get arr j) (j-1)
      else value in 
   Int.to_float (loop 0 (len - 1))

(* TODO é diferente da funcao uint_from_be_bytes se o primeiro byte
 for superior a 127, nesse caso o loop começa com value = -128*)
(*let int_from_be_bytes (arr, n : int array * int) : int =
   Printf.printf "debug 42: int_from_be_bytes n: %d\n" n;
   let len = Array.length arr in
   let value = uint_from_be_bytes(arr, n) in 
   if (Array.get arr (len-n) < 128) then
      uint_from_be_bytes(arr, n)
   else value +*)

let float64_from_le_bytes (bytes : int64 array) : float =
  let res = ref 0L in
  for i = 0 to 7 do
     res := Int64.add !res (Int64.shift_left (Array.get bytes i) (i * 8))
  done;
  Int64.float_of_bits !res

let float64_from_be_bytes (bytes : int64 array) : float =
   let res = ref 0L in
   for i = 0 to 7 do
     res := Int64.add !res (Int64.shift_left (Array.get bytes i) ((7 - i) * 8))
   done;
   Int64.float_of_bits !res

let float32_from_le_bytes (bytes : int32 array) : float =
  let res = ref 0l in
  for i = 0 to 3 do
      (res := Int32.add !res (Int32.shift_left (Array.get bytes i) (i * 8));
      Printf.printf "in float32_from_le_bytes loop, i = %d; byte = %d; res = %f\n" i ((Int32.to_int (Array.get bytes i))) (Int32.float_of_bits !res))
  done;
  Int32.float_of_bits !res

let float32_from_be_bytes (bytes : int32 array) : float =
   let res = ref 0l in
   for i = 0 to 3 do
       res := Int32.add !res (Int32.shift_left (Array.get bytes i) ((3 - i) * 8))
   done;
   Int32.float_of_bits !res
type bopt = 
    | SCLogAnd 
    | SCLogOr 


let str_of_binopt (op : bopt) (s1 : string) (s2 : string) : string = 
  match op with 
  | SCLogAnd -> s1 ^ " &&& " ^ s2
  | SCLogOr  -> s1 ^ " ||| " ^ s2 
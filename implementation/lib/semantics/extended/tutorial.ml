
let rec even (n : int) : bool = 
  if (n == 0) 
    then true 
  else even (n-2)
let trim_ends (s : string) : string = 
  let s_len = String.length s in 
  if (s_len >= 2)
    then String.sub s 1 ((String.length s) - 2)
    else raise (Failure ("INVALID GLOBAL VAR: "^s))
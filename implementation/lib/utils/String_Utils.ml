let trim_ends (s : string) : string =
  let s_len = String.length s in
  if (s_len >= 2)
    then String.sub s 1 ((String.length s) - 2)
    else raise (Failure ("INVALID GLOBAL VAR: "^s))


let from_char_code = fun (n : int) : string ->
  let c = Char.chr n in
  String.make 1 c

let to_char_code = fun (s : string ) : int ->
  let c = Char.code (s.[0]) in
  c

let to_upper_case (s : string) : string =
  let s = String.uppercase_ascii s in
  s

let to_lower_case (s : string) : string =
  let s = String.lowercase_ascii s in
  s

let trim (s : string) : string =
  let s = String.trim s in
  s

let make_fresh_var_generator (pref : string) : (unit -> string) =
  let count = ref 0 in
  fun () -> let x = !count in
    count := x+1; pref ^ (string_of_int x)

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

let to_char_code_u = fun (s : string ) : int ->
  let c = Char.code (s.[0]) and sLen = (String.length s) in
    if (c <= 0x7f) || (sLen = 1) then c
    else
      let c2 = Char.code (s.[1]) in
        if c <= 0xdf then
          if (c2 > 0x7f) && (c2 <= 0xbf) then
            (Int.shift_left (Int.logand c 0x1f) 6) +
            (Int.logand (Char.code (s.[1])) 0x3f)
          else
            c
        else if c <= 0xef then
          if (sLen >= 3) then
            let c3 = Char.code (s.[2]) in
              if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) then
                (Int.shift_left (Int.logand c 0xf) 12) +
                (Int.shift_left (Int.logand (Char.code (s.[1])) 0x3f) 6) +
                (Int.logand (Char.code (s.[2])) 0x3f)
              else
                c
          else
            c
        else
          if (sLen >= 4) then
            let c3 = Char.code (s.[2]) and c4 = Char.code (s.[3]) in
              if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) && (c4 > 0x7f) && (c4 <= 0xbf) then
                (Int.shift_left (Int.logand c 0x7) 18) +
                (Int.shift_left (Int.logand (Char.code (s.[1])) 0x3f) 12) +
                (Int.shift_left (Int.logand (Char.code (s.[2])) 0x3f) 6) +
                (Int.logand (Char.code (s.[3])) 0x3f)
              else
                c
          else
            c

(* TODO: Check if following characters start with bits 10, if not return the first character *)
let s_nth_u = fun (s : string) (i : int) : string ->
  let rec loop s cur_i_u cur_i i =
    let c = Char.code (s.[cur_i]) in
      if c <= 0x7f then
        if cur_i_u = i then String.sub s cur_i 1
        else loop s (cur_i_u + 1) (cur_i + 1) i
      else if c <= 0xdf then
        if cur_i_u = i then String.sub s cur_i 2
        else loop s (cur_i_u + 1) (cur_i + 2) i
      else if c <= 0xef then
        if cur_i_u = i then String.sub s cur_i 3
        else loop s (cur_i_u + 1) (cur_i + 3) i
      else
        if cur_i_u = i then String.sub s cur_i 4
        else loop s (cur_i_u + 1) (cur_i + 4) i
  in loop s 0 0 i

(* TODO: Check if following characters start with bits 10, if not return the first character *)
let s_len_u = fun (s : string) : int ->
  let rec loop s cur_i_u cur_i =
    if cur_i >= (String.length s) then (cur_i_u) else
    let c = Char.code (s.[cur_i]) in
      if c <= 0x7f then loop s (cur_i_u + 1) (cur_i + 1)
      else if c <= 0xdf then loop s (cur_i_u + 1) (cur_i + 2)
      else if c <= 0xef then loop s (cur_i_u + 1) (cur_i + 3)
      else loop s (cur_i_u + 1) (cur_i + 4)
  in loop s 0 0

let to_upper_case (s : string) : string =
  let s = String.uppercase_ascii s in
  s

let to_lower_case (s : string) : string =
  let s = String.lowercase_ascii s in
  s

let trim (s : string) : string =
  let s = String.trim s in
  s

let chop_first_char (s : string) : string =
  String.sub s 1 ((String.length s) - 1)

let make_fresh_var_generator (pref : string) : (unit -> string) =
  let count = ref 0 in
  fun () -> let x = !count in
    count := x+1; pref ^ (string_of_int x)

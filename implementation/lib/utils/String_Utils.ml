let trim_ends (s : string) : string =
  let s_len = String.length s in
  if (s_len >= 2)
    then String.sub s 1 ((String.length s) - 2)
    else raise (Failure ("INVALID GLOBAL VAR: "^s))


let from_char_code = fun (n : int) : string ->
  let c = Char.chr n in
  String.make 1 c

let from_char_code_u = fun (n : int) : string ->
  let b = Buffer.create 16 in
    Buffer.add_utf_8_uchar b (Uchar.of_int n);
    Buffer.contents b

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

let s_nth_u = fun (s : string) (i : int) : string ->
  let rec loop s cur_i_u cur_i i =
    let c = Char.code (s.[cur_i]) and sLen = ((String.length s) - cur_i)  in
      if (c <= 0x7f) || (sLen = 1) then
        if cur_i_u = i then String.sub s cur_i 1
        else loop s (cur_i_u + 1) (cur_i + 1) i
      else
        let c2 = Char.code (s.[cur_i+1]) in
          if c <= 0xdf then
            if (c2 > 0x7f) && (c2 <= 0xbf) then
              if cur_i_u = i then String.sub s cur_i 2
              else loop s (cur_i_u + 1) (cur_i + 2) i
            else
              if cur_i_u = i then String.sub s cur_i 1
              else loop s (cur_i_u + 1) (cur_i + 1) i
          else if c <= 0xef then
            if (sLen >= 3) then
              let c3 = Char.code (s.[cur_i+2]) in
                if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) then
                  if cur_i_u = i then String.sub s cur_i 3
                  else loop s (cur_i_u + 1) (cur_i + 3) i
                else
                  if cur_i_u = i then String.sub s cur_i 1
                  else loop s (cur_i_u + 1) (cur_i + 1) i
            else
              if cur_i_u = i then String.sub s cur_i 1
              else loop s (cur_i_u + 1) (cur_i + 1) i
          else
            if (sLen >= 4) then
              let c3 = Char.code (s.[cur_i+2]) and c4 = Char.code (s.[cur_i+3]) in
                if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) && (c4 > 0x7f) && (c4 <= 0xbf) then
                  if cur_i_u = i then String.sub s cur_i 4
                  else loop s (cur_i_u + 1) (cur_i + 4) i
                else
                  if cur_i_u = i then String.sub s cur_i 1
                  else loop s (cur_i_u + 1) (cur_i + 1) i
            else
              if cur_i_u = i then String.sub s cur_i 1
              else loop s (cur_i_u + 1) (cur_i + 1) i
  in loop s 0 0 i

let s_len_u = fun (s : string) : int ->
  let rec loop s cur_i_u cur_i =
    if cur_i >= (String.length s) then (cur_i_u) else
    let c = Char.code (s.[cur_i]) and sLen = ((String.length s) - cur_i) in
      if (c <= 0x7f) || (sLen = 1) then
        loop s (cur_i_u + 1) (cur_i + 1)
      else
        let c2 = Char.code (s.[cur_i+1]) in
          if c <= 0xdf then
            if (c2 > 0x7f) && (c2 <= 0xbf) then
              loop s (cur_i_u + 1) (cur_i + 2)
            else
              loop s (cur_i_u + 1) (cur_i + 1)
          else if c <= 0xef then
            if (sLen >= 3) then
              let c3 = Char.code (s.[cur_i+2]) in
                if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) then
                  loop s (cur_i_u + 1) (cur_i + 3)
                else
                  loop s (cur_i_u + 1) (cur_i + 1)
            else
              loop s (cur_i_u + 1) (cur_i + 1)
          else
            if (sLen >= 4) then
              let c3 = Char.code (s.[cur_i+2]) and c4 = Char.code (s.[cur_i+3]) in
                if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) && (c4 > 0x7f) && (c4 <= 0xbf) then
                  loop s (cur_i_u + 1) (cur_i + 4)
                else
                  loop s (cur_i_u + 1) (cur_i + 1)
            else
              loop s (cur_i_u + 1) (cur_i + 1)
  in loop s 0 0

let s_substr_u = fun (s : string) (i_u : int) (len_u : int) : string ->
  let rec loop s cur_i_u cur_i i_u len_u =
    if cur_i_u = i_u then
      let rec loop' s cur_i len_u0 len_i len_u =
        if len_u0 = len_u then
          String.sub s cur_i len_i
        else let c = Char.code (s.[cur_i + len_i]) and sLen = ((String.length s) - cur_i - len_i) in
          if (c <= 0x7f) || (sLen = 1) then loop' s cur_i (len_u0 + 1) (len_i + 1) len_u
          else
            let c2 = Char.code (s.[cur_i+len_i+1]) in
              if c <= 0xdf then
                if (c2 > 0x7f) && (c2 <= 0xbf) then
                  loop' s cur_i (len_u0 + 1) (len_i + 2) len_u
                else
                  loop' s cur_i (len_u0 + 1) (len_i + 1) len_u
              else if c <= 0xef then
                if (sLen >= 3) then
                  let c3 = Char.code (s.[cur_i+len_i+2]) in
                    if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) then
                      loop' s cur_i (len_u0 + 1) (len_i + 3) len_u
                    else
                      loop' s cur_i (len_u0 + 1) (len_i + 1) len_u
                else
                  loop' s cur_i (len_u0 + 1) (len_i + 1) len_u
              else
                if (sLen >= 4) then
                  let c3 = Char.code (s.[cur_i+len_i+2]) and c4 = Char.code (s.[cur_i+len_i+3]) in
                    if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) && (c4 > 0x7f) && (c4 <= 0xbf) then
                      loop' s cur_i (len_u0 + 1) (len_i + 4) len_u
                    else
                      loop' s cur_i (len_u0 + 1) (len_i + 1) len_u
                else
                  loop' s cur_i (len_u0 + 1) (len_i + 1) len_u
      in loop' s cur_i 0 0 len_u
    else
      let c = Char.code (s.[cur_i]) and sLen = ((String.length s) - cur_i) in
        if (c <= 0x7f) || (sLen = 1) then loop s (cur_i_u + 1) (cur_i + 1) i_u len_u
        else
          let c2 = Char.code (s.[cur_i+1]) in
            if c <= 0xdf then
              if (c2 > 0x7f) && (c2 <= 0xbf) then
                loop s (cur_i_u + 1) (cur_i + 2) i_u len_u
              else
                loop s (cur_i_u + 1) (cur_i + 1) i_u len_u
            else if c <= 0xef then
              if (sLen >= 3) then
                let c3 = Char.code (s.[cur_i+2]) in
                  if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) then
                    loop s (cur_i_u + 1) (cur_i + 3) i_u len_u
                  else
                    loop s (cur_i_u + 1) (cur_i + 1) i_u len_u
              else
                loop s (cur_i_u + 1) (cur_i + 1) i_u len_u
            else
              if (sLen >= 4) then
                let c3 = Char.code (s.[cur_i+2]) and c4 = Char.code (s.[cur_i+3]) in
                  if (c2 > 0x7f) && (c2 <= 0xbf) && (c3 > 0x7f) && (c3 <= 0xbf) && (c4 > 0x7f) && (c4 <= 0xbf) then
                    loop s (cur_i_u + 1) (cur_i + 4) i_u len_u
                  else
                    loop s (cur_i_u + 1) (cur_i + 1) i_u len_u
              else
                loop s (cur_i_u + 1) (cur_i + 1) i_u len_u
  in loop s 0 0 i_u len_u

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

(* Taken from: https://stackoverflow.com/a/42431362/3049315 *)
let utf8encode s =
  let prefs = [| 0x0; 0xc0; 0xe0 |] in
  let s1 n = String.make 1 (Char.chr n) in
  let rec ienc k sofar resid =
      let bct = if k = 0 then 7 else 6 - k in
      if resid < 1 lsl bct then
          (s1 (prefs.(k) + resid)) ^ sofar
      else
          ienc (k + 1) (s1 (0x80 + resid mod 64) ^ sofar) (resid / 64)
  in
  ienc 0 "" (Stdlib.int_of_string ("0x" ^ s))

let utf8decode s =
  let re = Str.regexp "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]" in
    let subst = function
    | Str.Delim u -> utf8encode (String.sub u 2 4)
    | Str.Text t -> t
    in
    String.concat "" (List.map subst (Str.full_split re s))

let hexdecode s =
  from_char_code_u (Stdlib.int_of_string ("0x" ^ (String.sub s 2 2)))
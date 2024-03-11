(*YYYY-MM-DDTHH:mm:ss.sssZ*)

let is_final_index str i =
  Printf.printf "debug 42: is_final_index %s, %d\n" str i;
  Printf.printf "debug 42: is_final_index %b \n" (String.length str = i);
  String.length str = i

let cl2s cl = String.concat "" (List.map (String.make 1) cl)

let is_timezone str i =
  Printf.printf "debug 42: is_timezone %s %d %b \n" str i
    (String.get str i <= 'Z' && String.get str i >= 'A');
  String.get str i <= 'Z' && String.get str i >= 'A'

let parse_segment (str : string) (i : int) (prev : char) (delim : char) :
  (float * int) option =
  Printf.printf "debug 42: parse_segment %s, %d %c %c\n" str i prev delim;
  let len = String.length str in
  if String.length str = i + 1 && String.get str i = prev then None
  else
    let rec loop (chrs : char list) (j : int) =
      if j >= len || String.get str j = delim then Some (chrs, j)
      else
        let cj = String.get str j in
        Printf.printf "debug 42: parse_segment char: %c\n" cj;
        if cj <= '9' && cj >= '0' then loop (chrs @ [ cj ]) (j + 1)
        else if cj <= 'Z' && cj >= 'A' then Some (chrs, j)
        else None
    in
    match loop [] (i + 1) with
    | None -> None
    | Some ([], _) -> None
    | Some (chrs, j) ->
      let str_number = cl2s chrs in
      let number = float_of_string str_number in
      Printf.printf "debug42: got number: %f\n" number;
      Some (number, j)

let parse_year (str : string) (i : int) : (float * int) option =
  Printf.printf "debug 42: parse_year %s, %d\n" str i;
  let len = String.length str in
  let rec loop (chrs : char list) (j : int) =
    if j >= len || String.get str j = '-' then Some (chrs, j)
    else
      let cj = String.get str j in
      Printf.printf "debug 42: parse_year char: %c\n" cj;
      if cj <= '9' && cj >= '0' then loop (chrs @ [ cj ]) (j + 1) else None
  in
  match loop [] i with
  | None -> None
  | Some (chrs, j) ->
    let str_year = cl2s chrs in
    let year = float_of_string str_year in
    Printf.printf "debug42: got year: %f\n" year;
    Some (year, j)

let parse_month (str : string) (i : int) : (float * int) option =
  parse_segment str i '-' '-'

let parse_day (str : string) (i : int) : (float * int) option =
  parse_segment str i '-' 'T'

let parse_hour (str : string) (i : int) : (float * int) option =
  parse_segment str i 'T' ':'

let parse_min (str : string) (i : int) : (float * int) option =
  parse_segment str i ':' ':'

let parse_sec (str : string) (i : int) : (float * int) option =
  parse_segment str i ':' '.'

let parse_msec (str : string) (i : int) : (float * int) option =
  parse_segment str i '.' 'Z' (* TODO *)

let parse_time_zone (str : string) (i : int) : string option =
  Some (str ^ string_of_int i)

let parse_sth (str : string) (fs : (string -> int -> (float * int) option) list)
  : (float list * int) option =
  Printf.printf "debug 42: parse_sth %s\n" str;
  List.fold_left
    (fun ac f ->
      match ac with
      | None -> None
      | Some (ns, i) -> (
        let ret = f str i in
        Printf.printf "debug 42: parse_sth indice: %d\n" i;
        match ret with
        | None when is_final_index str i || is_timezone str i ->
          Printf.printf "debug 42: parse_sth in first case\n";
          Some (ns @ [ Float.nan ], i)
        | None ->
          Printf.printf "debug 42: parse_sth in none case\n";
          None
        | Some (n, i') ->
          Printf.printf "debug 42: parse_sth in last case\n";
          Some (ns @ [ n ], i') ) )
    (Some ([], 0))
    fs

(*YYYY-MM-DDTHH:mm:ss.sssZ*)

let parse_date (date_str : string) : (float list * string) option =
  let ret =
    parse_sth date_str
      [ parse_year
      ; parse_month
      ; parse_day
      ; parse_hour
      ; parse_min
      ; parse_sec
      ; parse_msec
      ]
  in
  (*Printf.printf "parsed year: %f\n" (List.nth ret 0); *)
  match ret with
  | None -> None
  | Some (ns, i) -> (
    let zone = parse_time_zone date_str i in
    match zone with
    | None -> None
    | Some zone -> Some (ns, zone) )

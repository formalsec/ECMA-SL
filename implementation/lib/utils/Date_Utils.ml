
(*YYYY-MM-DDTHH:mm:ss.sssZ*)

let is_final_index str i = String.length str = i 

let cl2s cl = String.concat "" (List.map (String.make 1) cl)


let parse_year (str : string) (i : int) : (float * int) option = 
    let len = String.length str in 
    let rec loop (chrs : char list) (j : int) = 
        if (j >= len) || ((String.get str j) = '-')
            then Some (chrs, j)
            else (
                let cj = String.get str j in 
                (if cj <= '9' && cj >= '0' 
                    then loop (chrs @ [ cj ]) (j+1)
                    else None)) in 
    match loop [] i with 
    | None -> None 
    | Some (chrs, j) ->  
        let str_year = cl2s chrs in 
        Some (float_of_string str_year, j)

let parse_month (str : string) (i : int) : (float * int) option = 
    None 

let parse_day (str : string) (i : int) : (float * int) option = 
    None 

let parse_hour (str : string) (i : int) : (float * int) option = 
    None 

let parse_min (str : string) (i : int) : (float * int) option = 
    None 

let parse_sec (str : string) (i : int) : (float * int) option = 
    None 

let parse_msec (str : string) (i : int) : (float * int) option = 
    None 

let parse_time_zone (str : string) (i : int) : string option = 
    None

let parse_sth (str : string) (fs : (string -> int -> (float * int) option) list) : (float list * int) option =
    List.fold_left (fun ac f ->  
        match ac with 
        | None -> None 
        | Some (ns, i) -> 
            let ret = f str i in 
            (match ret with 
            | None when is_final_index str i -> Some (ns @ [ 0. ], i)
            | None -> None 
            | Some (n, i') -> Some (ns @ [ n ], i'))
    ) (Some ([], 0)) fs 


(*YYYY-MM-DDTHH:mm:ss.sssZ*)

let parse_date (date_str : string) : ((float list) * string) option = 
    let ret = parse_sth date_str [parse_year; parse_month; parse_day; parse_hour; parse_min; parse_sec; parse_msec ] in 
    match ret with 
    | None -> None 
    | Some (ns, i) -> 
      let zone = parse_time_zone date_str i in 
      (match zone with 
      | None -> None 
      | Some zone -> Some (ns, zone))
      


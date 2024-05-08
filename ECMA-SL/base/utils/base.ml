let default_hashtbl_sz = ref 16

type counter = (unit -> int) * (unit -> unit)

let time () : float = Unix.gettimeofday ()

let make_counter (init : int) (step : int) : counter =
  let counter = ref init in
  let next () =
    let n = !counter in
    counter := n + step;
    n
  and reset () = counter := init in
  (next, reset)

let make_name_generator (base : string) : unit -> string =
  let (next, _) = make_counter 0 1 in
  fun () -> base ^ string_of_int (next ())

let format_time (time : float) : int * int =
  let secs = int_of_float @@ floor time in
  let millis = int_of_float @@ Float.round (time *. 1000.0) in
  (secs, millis)

let format_bytes (bytes : int) : float * string =
  let units = [| "bytes"; "kb"; "mb"; "gb"; "tb" |] in
  let rec expbt sz i = if sz < 1024 then i else expbt (sz / 1024) (i + 1) in
  let i = expbt bytes 0 in
  (float_of_int bytes /. (1024.0 ** float_of_int i), units.(i))

type counter = (unit -> int) * (unit -> unit)

let make_counter (init : int) (step : int) : counter =
  let counter = ref init in
  let next () =
    let n = !counter in
    counter := n + step;
    n
  and reset () = counter := init in
  (next, reset)

let make_name_generator (pref : string) : unit -> string =
  let (next, _) = make_counter 0 1 in
  fun () -> Fmt.sprintf "%s%d" pref (next ())

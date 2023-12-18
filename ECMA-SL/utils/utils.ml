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

let js2ecma_sl ?id file output =
  let open Bos in
  let cmd = Cmd.(v "js2ecma-sl" % "-c" % "-i" % p file % "-o" % p output) in
  Option.fold ~none:cmd ~some:(fun fid -> Cmd.(add_arg cmd fid)) id

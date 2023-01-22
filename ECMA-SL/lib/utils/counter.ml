type counter = (unit -> int) * (unit -> unit)

let count (init : int) (step : int) : counter =
  let counter = ref init in
  let next () =
    let n = !counter in
    counter := n + step;
    n
  and reset () = counter := init in
  (next, reset)

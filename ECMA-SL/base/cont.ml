type 'a t = ('a -> unit) -> unit

let[@inline] empty : 'a t = fun _ -> ()
let[@inline] return (v : 'a) : 'a t = fun (k : 'a -> unit) -> k v

let[@inline] cons (hd : 'a) (tl : 'a t) =
 fun (k : 'a -> unit) ->
  let () = k hd in
  tl k

let[@inline] map (f : 'a -> 'b) (v : 'a t) =
 fun (k : 'b -> unit) -> v (fun (x : 'a) -> k (f x))

let[@inline] bind (f : 'a -> 'b t) (v : 'a t) =
 fun (k : 'b -> unit) -> v (fun (x : 'a) -> (f x) k)

let of_list (l : 'a list) = fun (k : 'a -> unit) -> List.iter k l

module Make () = struct
  exception Error of Source.region * string

  let warn at m = prerr_endline (Source.str at ^ ": warning: " ^ m)
  let error at m = raise (Error (at, m))
end

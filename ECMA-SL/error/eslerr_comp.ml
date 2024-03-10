open EslCore

module ErrSrc = struct
  open Source

  type t =
    | Region of region
    | Index of int

  let none () = Region no_region
  let at (el : 'a phrase) : t = Region el.at
  let region (region : region) : t = Region region
  let index (index : int) : t = Index index
end

module RtTrace = struct
  type t = Fmt.t -> unit -> unit
end

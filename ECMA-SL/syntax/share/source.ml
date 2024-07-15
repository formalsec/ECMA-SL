open EslBase

type pos =
  { line : int
  ; column : int
  }

type region =
  { file : string
  ; left : pos
  ; right : pos
  ; real : bool
  }

type +'a phrase =
  { it : 'a
  ; at : region
  }

let no_pos : pos = { line = -1; column = -1 }

let no_region : region =
  { file = ""; left = no_pos; right = no_pos; real = false }

let ( @> ) (it : 'a) (at : region) : 'a phrase = { it; at }

let ( @?> ) (it : 'a) (at : region) : 'a phrase =
  { it; at = { at with real = false } }

let map (f : 'a -> 'b) (x : 'a phrase) : 'b phrase = { x with it = f x.it }

let pp_pos (ppf : Fmt.t) (pos : pos) : unit =
  let pp_pos' ppf v = Fmt.(if v == -1 then pp_str ppf "x" else pp_int ppf v) in
  Fmt.fmt ppf "%a.%a" pp_pos' pos.line pp_pos' pos.column

let pp_region (ppf : Fmt.t) (at : region) : unit =
  Fmt.fmt ppf "%S:%a-%a" at.file pp_pos at.left pp_pos at.right

let pp (ppf : Fmt.t) (x : 'a phrase) = Fmt.fmt ppf "%a" pp_region x.at
let str (x : 'a phrase) : string = Fmt.str "%a" pp x

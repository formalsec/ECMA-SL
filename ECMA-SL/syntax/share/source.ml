open EslBase

type pos =
  { line : int
  ; col : int
  }

type at =
  { file : string
  ; lpos : pos
  ; rpos : pos
  ; real : bool
  }

type +'a t =
  { it : 'a
  ; at : at
  }

let pos_none : pos = { line = -1; col = -1 }
let none : at = { file = ""; lpos = pos_none; rpos = pos_none; real = false }
let ( @> ) (it : 'a) (at : at) : 'a t = { it; at } [@@inline]
let ( @?> ) (it : 'a) (at : at) : 'a t = it @> { at with real = false }
let map (f : 'a -> 'b) (x : 'a t) : 'b t = { x with it = f x.it } [@@inline]
let is_none (at : at) : bool = at = none [@@inline]

let pp_pos (ppf : Fmt.t) (pos : pos) : unit =
  let pp_pos' ppf v = Fmt.(if v == -1 then pp_str ppf "x" else pp_int ppf v) in
  Fmt.fmt ppf "%a.%a" pp_pos' pos.line pp_pos' pos.col

let pp_at (ppf : Fmt.t) (at : at) : unit =
  Fmt.fmt ppf "%S:%a-%a" at.file pp_pos at.lpos pp_pos at.rpos

let pp (ppf : Fmt.t) (x : 'a t) = Fmt.fmt ppf "%a" pp_at x.at [@@inline]
let str (x : 'a t) : string = Fmt.str "%a" pp x [@@inline]

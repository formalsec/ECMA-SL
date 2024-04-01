include Curses
open EslBase

let ( !! ) (err : bool) =
  if err == false then Log.err "Error in the Debugger TUI"

let proportional_sz ?(remainder : bool = false) (total : int) (factor : int)
  (weight : int) : int =
  let totalf = float_of_int total in
  let factorf = float_of_int factor in
  let weightf = float_of_int weight in
  let psz = weightf *. totalf /. factorf in
  int_of_float (if remainder then ceil psz else floor psz)

module Win = struct
  type t =
    { w : window
    ; x : int
    ; y : int
    ; xz : int
    ; yz : int
    }

  let mk (basewin : t) (x : int) (y : int) (xz : int) (yz : int) : t =
    let w = derwin basewin.w yz xz y x in
    { w; x; y; xz; yz }

  let refresh (win : t) : unit = !!(wrefresh win.w)
end

module FrameWin = struct
  type frame =
    { t : chtype
    ; l : chtype
    ; b : chtype
    ; r : chtype
    ; tl : chtype
    ; tr : chtype
    ; bl : chtype
    ; br : chtype
    }

  type t =
    { wregion : Win.t
    ; wframe : Win.t
    ; frame : frame
    }

  let default_frame (acs : Acs.acs) : frame =
    { t = acs.hline
    ; l = acs.vline
    ; b = acs.hline
    ; r = acs.vline
    ; tl = acs.ulcorner
    ; tr = acs.urcorner
    ; bl = acs.llcorner
    ; br = acs.lrcorner
    }

  let mk ?(frame : frame option) (acs : Acs.acs) (basewin : Win.t) (x : int)
    (y : int) (xz : int) (yz : int) : t =
    let wbox = Win.mk basewin x y xz yz in
    let wregion = Win.mk wbox 1 1 (xz - 2) (yz - 2) in
    let frame = Option.value ~default:(default_frame acs) frame in
    { wregion; wframe = wbox; frame }

  let draw (framewin : t) : unit =
    let f = framewin.frame in
    wborder framewin.wframe.w f.l f.r f.t f.b f.tl f.tr f.bl f.br

  let refresh (framewin : t) : unit =
    !!(wrefresh framewin.wframe.w);
    !!(wrefresh framewin.wregion.w)
end

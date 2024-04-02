include Curses
open EslBase

let ( !! ) (err : bool) : unit =
  if err == false then (
    endwin ();
    Internal_error.(throw __FUNCTION__ (Custom "error in the debugger TUI")) )

let proportional_sz ?(remainder : bool = false) (total : int) (factor : int)
  (weight : int) : int =
  let totalf = float_of_int total in
  let factorf = float_of_int factor in
  let weightf = float_of_int weight in
  let psz = weightf *. totalf /. factorf in
  int_of_float (if remainder then ceil psz else floor psz)

let draw_rectangle (w : window) (y : int) (x : int) (yz : int) (xz : int)
  (fill : char) : unit =
  let range (f : int) (l : int) = List.init (l - f) (fun i -> f + i) in
  let row fill' y' = mvwhline w y' x fill' xz in
  List.iter (row (Char.code fill)) (range y (y + yz))

type 'a element =
  { data : 'a
  ; window : 'a -> window
  ; refresh : 'a -> unit
  }

module Win = struct
  type t =
    { w : window
    ; y : int
    ; x : int
    ; yz : int
    ; xz : int
    }

  let mk (basewin : t) (y : int) (x : int) (yz : int) (xz : int) : t =
    let w = derwin basewin.w yz xz y x in
    { w; y; x; yz; xz }

  let window (win : t) : window = win.w
  let refresh (win : t) : unit = !!(wrefresh win.w)
  let element (win : t) : t element = { data = win; window; refresh }
end

module Frame = struct
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

  type 'a t =
    { frame : frame
    ; wframe : Win.t
    ; el : 'a element
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

  let mk ?(frame : frame option) (acs : Acs.acs) (basewin : Win.t) (y : int)
    (x : int) (yz : int) (xz : int) (el : 'a element) : 'a t =
    let frame = Option.value ~default:(default_frame acs) frame in
    let wframe = Win.mk basewin y x yz xz in
    { frame; wframe; el }

  let window (framewin : 'a t) : window = framewin.wframe.w

  let draw (framewin : 'a t) : unit =
    let f = framewin.frame in
    wborder framewin.wframe.w f.l f.r f.t f.b f.tl f.tr f.bl f.br

  let refresh (framewin : 'a t) : unit =
    !!(wrefresh framewin.wframe.w);
    framewin.el.refresh framewin.el.data

  let element (framewin : 'a t) : 'a t element =
    { data = framewin; window; refresh }
end

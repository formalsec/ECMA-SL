type t = (string, SecObject.t) Hashtbl.t

let create () : t = Hashtbl.create 511

let insert  (heap : t) (secobj : SecObject.t) : Loc.t =
  let loc = Loc.newloc () in
  Hashtbl.add heap loc secobj;
  loc

let remove (heap : t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : t) (loc : Loc.t) (secobj : SecObject.t) : unit = Hashtbl.replace heap loc secobj

let get (heap : t) (loc : Loc.t) : SecObject.t option = Hashtbl.find_opt heap loc


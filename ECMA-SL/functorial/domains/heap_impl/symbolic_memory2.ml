module V = Symbolic_value.M
open Syntax.Option

module Make
    (Object : Object_intf.S2
                with type value = V.value
                 and type value2 = Encoding.Expr.t) : Memory_intf.S2 = struct
  type value = Object.value
  type value2 = Object.value2
  type object_ = Object.t

  type t =
    { parent : t option
    ; map : (Loc.t, object_) Hashtbl.t
    }

  let create () = { parent = None; map = Hashtbl.create 16 }
  let clone h = { parent = Some h; map = Hashtbl.create 16 }

  let insert { map; _ } obj =
    let loc = Loc.create () in
    Hashtbl.add map loc obj;
    V.Val (Val.Loc loc)

  let remove h l = Hashtbl.remove h.map l
  let set h key data = Hashtbl.replace h.map key data

  let find memory l =
    let rec aux { parent; map } l from_parent =
      match Hashtbl.find_opt map l with
      | Some o -> Some (o, from_parent)
      | None ->
        let* parent in
        aux parent l true
    in
    aux memory l false

  let get memory l =
    let+ (obj, from_parent) = find memory l in
    if not from_parent then obj
    else
      let obj = Object.clone obj in
      set memory l obj;
      obj

  let get_field heap l field solver pc =
    let obj = get heap l in
    let res = Option.bind obj (fun o -> Some (Object.get o field solver pc)) in
    match res with
    | None -> Log.err "get Return is never none. loc: %s %a" l V.Pp.pp field
    | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc, v) ] ->
        set heap l obj;
        [ (heap, pc, v) ]
      | _ ->
        List.map
          (fun (obj, pc, v) ->
            let heap' = clone heap in
            set heap' l obj;
            (heap', pc, v) )
          objs )

  let has_field heap loc field solver pc =
    let res = get_field heap loc field solver pc in
    List.map
      (fun (new_heap, new_pc, v) ->
        let v' = V.Val (Val.Bool (Option.is_some v)) in
        (new_heap, new_pc, v') )
      res

  let set_field heap loc ~field ~data solver pc =
    let obj = get heap loc in
    let res =
      Option.bind obj (fun o -> Some (Object.set o ~key:field ~data solver pc))
    in
    match res with
    | None -> Log.err "set Return is never none. loc: %s" loc
    | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc) ] ->
        set heap loc obj;
        [ (heap, pc) ]
      | _ ->
        List.map
          (fun (obj, pc) ->
            let heap' = clone heap in
            set heap' loc obj;
            (heap', pc) )
          objs )

  let delete_field heap loc field solver pc =
    let obj = get heap loc in
    let res =
      Option.bind obj (fun o -> Some (Object.delete o field solver pc))
    in
    match res with
    | None -> Log.err "delete Return is never none. loc: %s" loc
    | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc') ] ->
        set heap loc obj;
        [ (heap, pc') ]
      | _ ->
        List.map
          (fun (obj, pc) ->
            let heap' = clone heap in
            set heap' loc obj;
            (heap', pc) )
          objs )
end

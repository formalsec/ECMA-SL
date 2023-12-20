open Stmt
open Source

exception Except of string

module StringSet = Set.Make (String)

type t =
  { name : string
  ; params : string list
  ; body : Stmt.t
  }

let create_store (func : t) (vals : Val.t list) : Val.t Store.t =
  let varvals = List.combine func.params vals in
  Store.create varvals

let print_list (lis : string list) : string = String.concat ", " lis

let create (name : string) (params : string list) (body : Stmt.t) : t =
  { name; params; body }

let get_name (func : t) : string = func.name
let get_params (func : t) : string list = func.params
let get_body (func : t) : Stmt.t = func.body

let str (func : t) : string =
  "function "
  ^ func.name
  ^ " ("
  ^ print_list func.params
  ^ ") { "
  ^ Stmt.str func.body
  ^ " }"

let to_json (func : t) : string =
  Printf.sprintf
    "{\"type\" : \"function\", \"name\" : \"%s\", \"params\" : [ %s ], \
     \"body\" :  %s }"
    func.name
    (String.concat ", "
       (List.map (fun str -> Printf.sprintf "\"%s\"" str) func.params) )
    (Stmt.to_json func.body)

let rec asgn_search (stmts : Stmt.t list) : StringSet.t =
  let set =
    List.fold_left
      (fun ac stmts ->
        match stmts.it with
        | Stmt.Assign (x, _e) -> StringSet.add x ac
        | Stmt.If (_e, _s1, Some _s2) -> (
          match (_s1.it, _s2.it) with
          | (Block s1, Block s2) ->
            StringSet.union
              (StringSet.union (asgn_search s1) (asgn_search s2))
              ac
          | (_, _) -> ac )
        | Stmt.If (_e, _s1, None) -> (
          match _s1.it with
          | Block s1 -> StringSet.union (asgn_search s1) ac
          | _ -> ac )
        | Stmt.While (_e, _s) -> (
          match _s.it with
          | Block s -> StringSet.union (asgn_search s) ac
          | _ -> ac )
        | FieldLookup (x, _e_o, _e_f) -> StringSet.add x ac
        | AssignNewObj x -> StringSet.add x ac
        | AssignInObjCheck (x, _e_o, _e_f) -> StringSet.add x ac
        | AssignObjToList (x, _e) -> StringSet.add x ac
        | AssignObjFields (x, _e) -> StringSet.add x ac
        | AssignCall (x, _, _) -> StringSet.add x ac
        | _ -> ac )
      StringSet.empty stmts
  in
  set

let asgn_vars (stmt : Stmt.t) : string list =
  match stmt.it with
  | Block stmts ->
    let set = asgn_search stmts in
    let list = StringSet.to_seq set in
    List.of_seq list
  | _ -> raise (Except "Func.asgn_vars not a block!")

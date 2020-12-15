let _SHADOW_FUN_NAME_ = "shadow"
let _SHADOW_PROP_EXISTS_ = "shadowPropExists"
let _SHADOW_PROP_VALUE_ = "shadowPropValue"
let _OBJ_STRUCT_LEV_PROP_ = "structLev"
let _OBJ_VALUE_LEV_PROP_ = "objLev"
let _INITIAL_PC_ = "low"
let _upgVar_ = "upgVar"
let _upgPropExists_ = "upgPropExists"
let _upgPropVal_ = "upgPropVal"
let _upgStruct_ = "upgStruct"
let _upgObject_ = "upgObject"
let _PARSE_LVL_ = "parse_lvl"

let _LUB_ = "lub"
let _LUBN_ = "lubn"
let _LEQ_ = "leq"

let mk_fresh_var (str : string) : (unit -> string)=
  let counter = ref 0 in
  let rec f () =
    (let v = str ^ (string_of_int !counter) in
    counter := !counter +1;
    v)
  in f

let fresh_var  = mk_fresh_var "_freshvar_"
let fresh_var_lev = mk_fresh_var "_freshvar_lev_"
let fresh_expr_lev = mk_fresh_var "_fresh_lev_var"
let fresh_obj  = mk_fresh_var "_fresh_obj_"
let fresh_field  = mk_fresh_var "_fresh_field_"
let fresh_field_lev  = mk_fresh_var "_fresh_field_lev_"

let mk_fresh_pc (): (unit -> string)=
  let counterpc = ref 1 in
  let rec f () =
    (let v = "_pc_" ^ (string_of_int !counterpc) in
    counterpc := !counterpc +1;
    v)
  in f

let  fresh_pc  = mk_fresh_pc ()

let shadowvar (var:string):string = 
    (var ^ "_lev")
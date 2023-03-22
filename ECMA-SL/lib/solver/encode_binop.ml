let encode_gt (v_type : Type.t option) (ctx : Z3.context) : 
  Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr = 
  match v_type with
  | Some t -> (
    match t with
    | Type.IntType -> Z3.Arithmetic.mk_gt ctx
    | Type.FltType -> Z3.FloatingPoint.mk_gt ctx
    | _ -> failwith "Encoding: gt argument type unexpected."
  )
  | None -> failwith "Encoding: gt argument has no type."


let encode_lt (v_type : Type.t option) (ctx : Z3.context) : 
  Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr = 
  match v_type with
  | Some t -> (
    match t with
    | Type.IntType -> Z3.Arithmetic.mk_lt ctx
    | Type.FltType -> Z3.FloatingPoint.mk_lt ctx
    | _ -> failwith "Encoding: lt argument type unexpected."
  )
  | None -> failwith "Encoding: lt argument has no type."


let encode_ge (v_type : Type.t option) (ctx : Z3.context) : 
  Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr = 
  match v_type with
  | Some t -> (
    match t with
    | Type.IntType -> Z3.Arithmetic.mk_ge ctx
    | Type.FltType -> Z3.FloatingPoint.mk_geq ctx
    | _ -> failwith "Encoding: GE argument type unexpected."
  )
  | None -> failwith "Encoding: GE argument has no type."

let encode_le (v_type : Type.t option) (ctx : Z3.context) : 
  Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr = 
  match v_type with
  | Some t ->(
    match t with
    | Type.IntType -> Z3.Arithmetic.mk_le ctx
    | Type.FltType -> Z3.FloatingPoint.mk_leq ctx
    | _ -> failwith "Encoding: LE argument type unexpected."
  )
  | None -> failwith "Encoding: LE argument has no type."

let encode_add (v_type : Type.t option) (ctx : Z3.context) (rne : Z3.Expr.expr): 
  Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr = 
  match v_type with
  | Some t ->(
    match t with
    | Type.IntType -> fun v1 v2 -> Z3.Arithmetic.mk_add ctx [ v1; v2 ]
    | Type.FltType -> Z3.FloatingPoint.mk_add ctx rne
    | _ -> failwith "Encoding: plus argument type unexpected."
  )
  | None -> failwith "Encoding: plus argument has no type."

let encode_times (v_type : Type.t option) (ctx : Z3.context) (rne : Z3.Expr.expr): 
  Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr = 
  match v_type with
  | Some t ->(
    match t with
    | Type.IntType -> fun v1 v2 -> Z3.Arithmetic.mk_mul ctx [ v1; v2 ]
    | Type.FltType -> Z3.FloatingPoint.mk_mul ctx rne
    | _ -> failwith "Encoding: mul argument type unexpected."
  )
  | None -> failwith "Encoding: mul argument has no type."

let encode_div (v_type : Type.t option) (ctx : Z3.context) (rne : Z3.Expr.expr): 
  Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr = 
  match v_type with
  | Some t ->(
    match t with
    | Type.IntType -> Z3.Arithmetic.mk_div ctx
    | Type.FltType -> Z3.FloatingPoint.mk_div ctx rne
    | _ -> failwith "Encoding: mul argument type unexpected."
  )
  | None -> failwith "Encoding: mul argument has no type."
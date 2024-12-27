open EslBase
module Env = Symbolic.P.Env
module SMap = Link_env.SMap

let of_array arr =
  Array.fold_left (fun m (k, v) -> SMap.add k v m) SMap.empty arr

module type S = sig
  type extern_func

  val extern_cmds : Env.t -> extern_func SMap.t
  val symbolic_api : Fpath.t -> extern_func SMap.t
  val concrete_api : extern_func SMap.t
end

module Make () = struct
  module Value = Symbolic.P.Value
  module Choice = Symbolic.P.Choice
  module Extern_func = Symbolic.P.Extern_func
  module Thread = Choice_monad.Thread
  module Optimizer = Choice_monad.Optimizer
  open Extern_func

  let ( let/ ) = Choice.bind
  let fresh_i = Base.make_name_generator "i"
  let fresh_x = Base.make_name_generator "x"
  let fresh_func = Base.make_name_generator "eval_func_"
  let ok v = Choice.return (Ok v)
  let ok_v v = ok @@ Smtml.Expr.value v
  let err v = Choice.return (Error (`Failure v))

  let extern_cmds env =
    let parseJS data =
      let open EslJSParser.Api in
      let data =
        match Smtml.Expr.view data with
        | Val (Str data) -> data
        | _ -> assert false
      in
      let input_file = Filename.temp_file "__parse_in_" "__.js" in
      let output_file = Filename.temp_file "__parse_out_" "__.js" in
      Io.write_file input_file data;
      let fid = fresh_func () in
      begin
        match Bos.OS.Cmd.run (cmd input_file (Some output_file) (Some fid)) with
        | Error (`Msg msg) -> Log.stderr "%s" msg
        | Ok () -> ()
      end;
      let data = Io.read_file output_file in
      let func = Parsing.parse_func data in
      Env.add_func env fid func;
      ok_v (Str fid)
    in
    of_array [| ("parseJS", Extern_func (Func (Arg Res), parseJS)) |]

  let concrete_api =
    let open External.Impl in
    let int_to_four_hex v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (int_to_four_hex v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let octal_to_decimal v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (octal_to_decimal v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let to_precision v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (to_precision (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let to_exponential v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (to_exponential (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let to_fixed v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (to_fixed (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let from_char_code v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (from_char_code v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let to_char_code v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (to_char_code v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let s_len v = ok @@ Smtml.Expr.unop Ty_str Length v in
    let s_concat v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (s_concat v)
      | List l -> ok @@ Smtml.Expr.naryop Ty_str Concat l
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let s_nth v1 v2 = ok @@ Smtml.Expr.binop Ty_str At v1 v2 in
    let s_substr v1 v2 v3 =
      ok @@ Smtml.Expr.triop Ty_str String_extract v1 v2 v3
    in
    let from_char_code_u v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (from_char_code_u v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let to_char_code_u v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (to_char_code_u v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let to_lower_case v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (to_lower_case v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let to_upper_case v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (to_upper_case v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let trim v = ok @@ Smtml.Expr.unop Ty_str Trim v in
    let s_len_u v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (s_len_u v)
      | _ -> ok @@ Smtml.Expr.unop Ty_str Length v
    in
    let s_nth_u v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (s_nth_u (v1, v2))
      | _ -> ok @@ Smtml.Expr.binop Ty_str At v1 v2
    in
    let s_split v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (s_split (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let s_substr_u v1 v2 v3 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2, Smtml.Expr.view v3) with
      | (Val v1, Val v2, Val v3) -> ok_v (s_substr_u (v1, v2, v3))
      | _ -> ok @@ Smtml.Expr.triop Ty_str String_extract v1 v2 v3
    in
    let array_len v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (array_len v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let array_make v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (array_make (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let array_nth v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (array_nth (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let array_set v1 v2 v3 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2, Smtml.Expr.view v3) with
      | (Val v1, Val v2, Val v3) -> ok_v (array_set (v1, v2, v3))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let l_len v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (l_len v)
      | List _ -> ok @@ Smtml.Expr.unop Ty_list Length v
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let l_reverse v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (l_reverse v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let l_nth v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (l_nth (v1, v2))
      | (List _, _) -> ok @@ Smtml.Expr.binop Ty_list At v1 v2
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let l_add v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (l_add (v1, v2))
      | (List _, _) ->
        ok
        @@ Smtml.Expr.binop Ty_list List_append v1
        @@ Smtml.Expr.make (List [ v2 ])
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let l_prepend v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (l_prepend (v1, v2))
      | (_, List _) -> ok @@ Smtml.Expr.binop Ty_list List_cons v1 v2
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let l_concat v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (l_concat (v1, v2))
      | _ -> ok @@ Smtml.Expr.binop Ty_list List_append v1 v2
    in
    let l_set v1 v2 v3 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2, Smtml.Expr.view v3) with
      | (Val v1, Val v2, Val v3) -> ok_v (l_set (v1, v2, v3))
      | _ -> ok @@ Smtml.Expr.triop Ty_list List_set v1 v2 v3
    in
    let list_to_array v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (list_to_array v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let list_sort v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (list_sort v)
      (* TODO:x | List lst -> ok_v (List (List.sort compare lst)) *)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let list_mem v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (list_mem (v1, v2))
      | (_, List lst) -> ok_v (if List.mem v1 lst then True else False)
      | _ ->
        err
          (Fmt.str "%s: invalid arguments: %a %a" __FUNCTION__ Smtml.Expr.pp v1
             Smtml.Expr.pp v2 )
    in
    let list_remove_last v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (list_remove_last v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let list_remove v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (list_remove (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let list_remove_nth v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (list_remove_nth (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float_to_byte v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float_to_byte v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float32_to_le_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float32_to_le_bytes v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float32_to_be_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float32_to_be_bytes v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float64_to_le_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float64_to_le_bytes v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float64_to_be_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float64_to_be_bytes v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float32_from_le_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float32_from_le_bytes v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float32_from_be_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float32_from_be_bytes v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float64_from_le_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float64_from_le_bytes v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let float64_from_be_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float64_from_be_bytes v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let bytes_to_string v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (bytes_to_string v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let int_to_be_bytes v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (int_to_be_bytes (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let int_from_le_bytes v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (int_from_le_bytes (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let uint_from_le_bytes v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (uint_from_le_bytes (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let log_2 v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (log_2 v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let log_e v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (log_e v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let log_10 v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (log_10 v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let sin v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (sin v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let cos v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (cos v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let tan v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (tan v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let sinh v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (sinh v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let cosh v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (cosh v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let tanh v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (tanh v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let asin v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (asin v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let acos v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (acos v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let atan v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (atan v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let atan2 v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (atan2 (v1, v2))
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let exp v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (exp v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let abs v = ok @@ Smtml.Expr.unop Ty_real Abs v in
    let sqrt v = ok @@ Smtml.Expr.unop Ty_real Sqrt v in
    let ceil v = ok @@ Smtml.Expr.unop Ty_real Ceil v in
    let floor v = ok @@ Smtml.Expr.unop Ty_real Floor v in
    let trunc v = ok @@ Smtml.Expr.unop Ty_real Trunc v in
    let utf8_decode v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (utf8_decode v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let hex_decode v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (hex_decode v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let parse_number v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (parse_number v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let parse_string v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (parse_string v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    let parse_date v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (parse_date v)
      | _ -> err (__FUNCTION__ ^ ": invalid argument")
    in
    of_array
      [| (* int *)
         ( "int_to_four_hex_external"
         , Extern_func (Func (Arg Res), int_to_four_hex) )
       ; ( "octal_to_decimal_external"
         , Extern_func (Func (Arg Res), octal_to_decimal) )
         (* float *)
       ; ( "to_precision_external"
         , Extern_func (Func (Arg (Arg Res)), to_precision) )
       ; ( "to_exponential_external"
         , Extern_func (Func (Arg (Arg Res)), to_exponential) )
       ; ("to_fixed_external", Extern_func (Func (Arg (Arg Res)), to_fixed))
         (* string *)
       ; ( "from_char_code_external"
         , Extern_func (Func (Arg Res), from_char_code) )
       ; ("to_char_code_external", Extern_func (Func (Arg Res), to_char_code))
       ; ("s_len_external", Extern_func (Func (Arg Res), s_len))
       ; ("s_concat_external", Extern_func (Func (Arg Res), s_concat))
       ; ("s_nth_external", Extern_func (Func (Arg (Arg Res)), s_nth))
       ; ( "s_substr_external"
         , Extern_func (Func (Arg (Arg (Arg Res))), s_substr) )
       ; ( "from_char_code_u_external"
         , Extern_func (Func (Arg Res), from_char_code_u) )
       ; ( "to_char_code_u_external"
         , Extern_func (Func (Arg Res), to_char_code_u) )
       ; ("to_lower_case_external", Extern_func (Func (Arg Res), to_lower_case))
       ; ("to_upper_case_external", Extern_func (Func (Arg Res), to_upper_case))
       ; ("trim_external", Extern_func (Func (Arg Res), trim))
       ; ("s_len_u_external", Extern_func (Func (Arg Res), s_len_u))
       ; ("s_nth_u_external", Extern_func (Func (Arg (Arg Res)), s_nth_u))
       ; ("s_split_external", Extern_func (Func (Arg (Arg Res)), s_split))
       ; ( "s_substr_u_external"
         , Extern_func (Func (Arg (Arg (Arg Res))), s_substr_u) )
         (* array *)
       ; ("a_len_external", Extern_func (Func (Arg Res), array_len))
       ; ("array_make_external", Extern_func (Func (Arg (Arg Res)), array_make))
       ; ("a_nth_external", Extern_func (Func (Arg (Arg Res)), array_nth))
       ; ("a_set_external", Extern_func (Func (Arg (Arg (Arg Res))), array_set))
         (* list *)
       ; ("l_len_external", Extern_func (Func (Arg Res), l_len))
       ; ("l_reverse_external", Extern_func (Func (Arg Res), l_reverse))
       ; ("l_nth_external", Extern_func (Func (Arg (Arg Res)), l_nth))
       ; ("l_add_external", Extern_func (Func (Arg (Arg Res)), l_add))
       ; ("l_prepend_external", Extern_func (Func (Arg (Arg Res)), l_prepend))
       ; ("l_concat_external", Extern_func (Func (Arg (Arg Res)), l_concat))
       ; ("l_set_external", Extern_func (Func (Arg (Arg (Arg Res))), l_set))
       ; ("list_to_array_external", Extern_func (Func (Arg Res), list_to_array))
       ; ("l_sort_external", Extern_func (Func (Arg Res), list_sort))
       ; ("in_list_external", Extern_func (Func (Arg (Arg Res)), list_mem))
       ; ( "l_remove_last_external"
         , Extern_func (Func (Arg Res), list_remove_last) )
       ; ("l_remove_external", Extern_func (Func (Arg (Arg Res)), list_remove))
       ; ( "l_remove_nth_external"
         , Extern_func (Func (Arg (Arg Res)), list_remove_nth) )
         (* byte *)
       ; ("float_to_byte_external", Extern_func (Func (Arg Res), float_to_byte))
       ; ( "float32_to_le_bytes_external"
         , Extern_func (Func (Arg Res), float32_to_le_bytes) )
       ; ( "float32_to_be_bytes_external"
         , Extern_func (Func (Arg Res), float32_to_be_bytes) )
       ; ( "float64_to_le_bytes_external"
         , Extern_func (Func (Arg Res), float64_to_le_bytes) )
       ; ( "float64_to_be_bytes_external"
         , Extern_func (Func (Arg Res), float64_to_be_bytes) )
       ; ( "float32_from_le_bytes_external"
         , Extern_func (Func (Arg Res), float32_from_le_bytes) )
       ; ( "float32_from_be_bytes_external"
         , Extern_func (Func (Arg Res), float32_from_be_bytes) )
       ; ( "float64_from_le_bytes_external"
         , Extern_func (Func (Arg Res), float64_from_le_bytes) )
       ; ( "float64_from_be_bytes_external"
         , Extern_func (Func (Arg Res), float64_from_be_bytes) )
       ; ( "bytes_to_string_external"
         , Extern_func (Func (Arg Res), bytes_to_string) )
       ; ( "int_to_be_bytes_external"
         , Extern_func (Func (Arg (Arg Res)), int_to_be_bytes) )
       ; ( "int_from_le_bytes_external"
         , Extern_func (Func (Arg (Arg Res)), int_from_le_bytes) )
       ; ( "uint_from_le_bytes_external"
         , Extern_func (Func (Arg (Arg Res)), uint_from_le_bytes) )
         (* math *)
       ; ("log_2_external", Extern_func (Func (Arg Res), log_2))
       ; ("log_e_external", Extern_func (Func (Arg Res), log_e))
       ; ("log_10_external", Extern_func (Func (Arg Res), log_10))
       ; ("sin_external", Extern_func (Func (Arg Res), sin))
       ; ("cos_external", Extern_func (Func (Arg Res), cos))
       ; ("tan_external", Extern_func (Func (Arg Res), tan))
       ; ("sinh_external", Extern_func (Func (Arg Res), sinh))
       ; ("cosh_external", Extern_func (Func (Arg Res), cosh))
       ; ("tanh_external", Extern_func (Func (Arg Res), tanh))
       ; ("asin_external", Extern_func (Func (Arg Res), asin))
       ; ("acos_external", Extern_func (Func (Arg Res), acos))
       ; ("atan_external", Extern_func (Func (Arg Res), atan))
       ; ("atan2_external", Extern_func (Func (Arg (Arg Res)), atan2))
       ; ("exp_external", Extern_func (Func (Arg Res), exp)) (* parse *)
       ; ("abs_external", Extern_func (Func (Arg Res), abs)) (* parse *)
       ; ("sqrt_external", Extern_func (Func (Arg Res), sqrt)) (* parse *)
       ; ("ceil_external", Extern_func (Func (Arg Res), ceil)) (* parse *)
       ; ("floor_external", Extern_func (Func (Arg Res), floor)) (* parse *)
       ; ("trunc_external", Extern_func (Func (Arg Res), trunc)) (* parse *)
       ; ("utf8_decode_external", Extern_func (Func (Arg Res), utf8_decode))
       ; ("hex_decode_external", Extern_func (Func (Arg Res), hex_decode))
       ; ("parse_number_external", Extern_func (Func (Arg Res), parse_number))
       ; ("parse_string_external", Extern_func (Func (Arg Res), parse_string))
       ; ("parse_date_external", Extern_func (Func (Arg Res), parse_date))
      |]

  let symbolic_api filename =
    let open Extern_func in
    let non_empty v =
      match Smtml.Expr.view v with
      | Val (Str "") -> fresh_x ()
      | Val (Str s) -> s
      | _ -> Log.fail "'%a' is not a valid string symbol" Value.pp v
    in
    let str_symbol (x : value) =
      ok (Smtml.Expr.symbol (Smtml.Symbol.make Ty_str (non_empty x)))
    in
    let int_symbol (x : value) = ok (Value.int_symbol_s (non_empty x)) in
    let flt_symbol (x : value) =
      ok (Smtml.Expr.symbol (Smtml.Symbol.make Ty_real (non_empty x)))
    in
    let bool_symbol (x : value) =
      ok (Smtml.Expr.symbol (Smtml.Symbol.make Ty_bool (non_empty x)))
    in
    let is_symbolic (n : value) =
      ok_v (if Value.is_symbolic n then True else False)
    in
    let is_number (n : value) =
      ok_v
        (* TODO:x check is this is right *)
        ( match Smtml.Expr.ty n with
        | Ty_int | Ty_real -> True
        | _ -> False )
    in
    let is_sat (e : value) =
      let/ b = Choice.check e in
      ok_v (if b then True else False)
    in
    let exec (e : value) =
      (* TODO: more fine-grained exploit analysis *)
      match Smtml.Expr.view e with
      | Val _ -> ok @@ Value.mk_symbol "undefined"
      | _ ->
        Choice.with_mutable_thread @@ fun thread ->
        let open Smtml.Expr in
        let q =
          binop Ty_str String_contains e (value (Str "`touch success`"))
        in
        Logs.app (fun m -> m "       exec : %a@." pp e);
        (Error (`Exec_failure e), Thread.add_pc thread q)
    in
    let eval (e : value) =
      match Smtml.Expr.view e with
      | Val _ -> ok @@ Value.mk_symbol "undefined"
      | _ ->
        Choice.with_mutable_thread @@ fun thread ->
        let open Smtml.Expr in
        let q =
          binop Ty_str String_contains e
            (value (Str ";console.log('success')//"))
        in
        Logs.app (fun m -> m "       eval : %a@." pp e);
        (Error (`Eval_failure e), Thread.add_pc thread q)
    in
    let read_file (e : value) =
      match Smtml.Expr.view e with
      | Val _ -> ok @@ Value.mk_symbol "undefined"
      | _ ->
        Choice.with_mutable_thread @@ fun thread ->
        let open Smtml.Expr in
        let q = binop Ty_str String_contains e (value (Str "./exploited")) in
        Logs.app (fun m -> m "   readFile : %a@." pp e);
        (Error (`ReadFile_failure e), Thread.add_pc thread q)
    in

    let abort (e : value) =
      let e' = Format.asprintf "%a" Value.pp e in
      Log.stdout "      abort : %s@." e';
      Choice.return @@ Error (`Abort e')
    in
    let assume (e : value) =
      match Smtml.Expr.view e with
      | Val False -> Choice.stop
      | _ ->
        Choice.with_mutable_thread @@ fun thread ->
        (Ok (Value.mk_symbol "undefined"), Thread.add_pc thread e)
    in
    let evaluate (e : value) =
      Choice.with_thread @@ fun thread ->
      let pc = Smtml.Expr.Set.add e @@ Thread.pc thread in
      let solver = Thread.solver thread in
      assert (`Sat = Solver.check_set solver pc);
      Ok (Solver.get_value solver e)
    in
    let optimize target opt e pc =
      Optimizer.push opt;
      Optimizer.add opt pc;
      let v = target opt e in
      Optimizer.pop opt;
      v
    in
    let maximize (e : value) =
      Choice.with_thread @@ fun thread ->
      let pc = Smtml.Expr.Set.to_list @@ Thread.pc thread in
      let opt = Thread.optimizer thread in
      let v = optimize Optimizer.maximize opt e pc in
      match v with
      | Some v -> Ok (Smtml.Expr.value v)
      | None ->
        (* TODO: Error here *)
        assert false
    in
    let minimize (e : value) =
      Choice.with_thread @@ fun thread ->
      let pc = Smtml.Expr.Set.to_list @@ Thread.pc thread in
      let opt = Thread.optimizer thread in
      let v = optimize Optimizer.minimize opt e pc in
      match v with
      | Some v -> Ok (Smtml.Expr.value v)
      | None ->
        (* TODO: Error here *)
        assert false
    in
    let print (v : Value.value) =
      Log.stdout "extern print: %a@." Value.pp v;
      ok_v (App (`Op "symbol", [ Str "undefined" ]))
    in
    (* TODO: The following functions where optimizations merged from the
       `trunk` branch. Check if we can integrate this with the concrete API. *)
    let str_replace (s : Value.value) (t : Value.value) (t' : Value.value) =
      ok @@ Smtml.Expr.triop Ty_str String_replace s t t'
    in
    let str_indexof (s : Value.value) (t : Value.value) (i : Value.value) =
      ok
      @@ Smtml.Expr.cvtop Ty_real Reinterpret_int
      @@ Smtml.Expr.triop Ty_str String_index s t
      @@ Smtml.Expr.cvtop Ty_int Reinterpret_float i
    in
    let str_lastIndexOf (s : Value.value) (t : Value.value) =
      ok
      @@ Smtml.Expr.cvtop Ty_real Reinterpret_int
      @@ Smtml.Expr.binop Ty_str String_last_index s t
    in
    let str_sub (s : Value.value) (start : Value.value) (len : Value.value) =
      ok @@ Smtml.Expr.triop Ty_str String_extract s start len
    in
    let str_parse_int (str : Value.value) =
      ok
      @@ Smtml.Expr.cvtop Ty_real Reinterpret_int
      @@ Smtml.Expr.cvtop Ty_str String_to_int str
    in
    let str_split (s : Value.value) (r : Value.value) =
      Choice.with_mutable_thread @@ fun thread ->
      let x1 = Smtml.Expr.symbol @@ Smtml.Symbol.make Ty_str @@ fresh_x () in
      let x2 = Smtml.Expr.symbol @@ Smtml.Symbol.make Ty_str @@ fresh_x () in
      let cond =
        Smtml.Expr.relop Ty_bool Eq s
        @@ Smtml.Expr.naryop Ty_str Concat [ x1; r; x2 ]
      in
      let result = Value.(mk_list [ x1; x2 ]) in
      (Ok result, Thread.add_pc thread cond)
    in
    let str_match (s : Value.value) =
      Choice.with_mutable_thread @@ fun thread ->
      let x = Smtml.Expr.symbol @@ Smtml.Symbol.make Ty_str @@ fresh_x () in
      let cond = Smtml.Expr.binop Ty_str String_contains s x in
      (Ok (Value.mk_list [ x ]), Thread.add_pc thread cond)
    in
    let dirname () =
      let (dirname, _) = Fpath.split_base filename in
      ok_v @@ Str (Fpath.to_string dirname)
    in
    let filename () =
      let filename = Fpath.to_string filename in
      ok_v @@ Str filename
    in
    of_array
      [| ("str_symbol", Extern_func (Func (Arg Res), str_symbol))
       ; ("int_symbol", Extern_func (Func (Arg Res), int_symbol))
       ; ("flt_symbol", Extern_func (Func (Arg Res), flt_symbol))
       ; ("bool_symbol", Extern_func (Func (Arg Res), bool_symbol))
       ; ("is_symbolic", Extern_func (Func (Arg Res), is_symbolic))
       ; ("is_number", Extern_func (Func (Arg Res), is_number))
       ; ("is_sat", Extern_func (Func (Arg Res), is_sat))
       ; ("exec", Extern_func (Func (Arg Res), exec))
       ; ("eval", Extern_func (Func (Arg Res), eval))
       ; ("readFile", Extern_func (Func (Arg Res), read_file))
       ; ("abort", Extern_func (Func (Arg Res), abort))
       ; ("assume", Extern_func (Func (Arg Res), assume))
       ; ("evaluate", Extern_func (Func (Arg Res), evaluate))
       ; ("maximize", Extern_func (Func (Arg Res), maximize))
       ; ("minimize", Extern_func (Func (Arg Res), minimize))
       ; ("value", Extern_func (Func (Arg Res), print))
       ; ("str_replace", Extern_func (Func (Arg (Arg (Arg Res))), str_replace))
       ; ("str_indexOf", Extern_func (Func (Arg (Arg (Arg Res))), str_indexof))
       ; ("str_lastIndexOf", Extern_func (Func (Arg (Arg Res)), str_lastIndexOf))
       ; ("str_sub", Extern_func (Func (Arg (Arg (Arg Res))), str_sub))
       ; ("str_parseInt", Extern_func (Func (Arg Res), str_parse_int))
       ; ("str_match", Extern_func (Func (Arg Res), str_match))
       ; ("__dirname", Extern_func (Func (UArg Res), dirname))
       ; ("__filename", Extern_func (Func (UArg Res), filename))
       ; ("summ_string_split", Extern_func (Func (Arg (Arg Res)), str_split))
      |]
end

include (Make () : S with type extern_func := Symbolic.P.Extern_func.extern_func)

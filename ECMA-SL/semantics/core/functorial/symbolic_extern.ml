open EslBase
open EslSyntax
module Env = Symbolic.P.Env
module SMap = Link_env.SMap
module PC = Choice_monad.PC

let of_array l = Array.fold_left (fun m (k, v) -> SMap.add k v m) SMap.empty l

module type S = sig
  type extern_func

  val extern_cmds : Env.t -> extern_func SMap.t
  val symbolic_api : extern_func SMap.t
  val concrete_api : extern_func SMap.t
end

module Make () = struct
  module Value = Symbolic.P.Value
  module Choice = Symbolic.P.Choice
  module Extern_func = Symbolic.P.Extern_func
  module Thread = Choice_monad.Thread
  module Translator = Value_translator
  module Optimizer = Choice_monad.Optimizer
  module PC = Choice_monad.PC

  let ( let/ ) = Choice.bind
  let fresh_i = Base.make_name_generator "i"
  let fresh_x = Base.make_name_generator "x"
  let fresh_func = Base.make_name_generator "eval_func_"

  let extern_cmds env =
    let open Extern_func in
    let parseJS data =
      let open EslJSParser.Api in
      let data =
        match data with Value.Val (Val.Str data) -> data | _ -> assert false
      in
      let input_file = Filename.temp_file "__parse_in_" "__.js" in
      let output_file = Filename.temp_file "__parse_out_" "__.js" in
      Io.write_file input_file data;
      let fid = fresh_func () in
      begin
        match Bos.OS.Cmd.run (cmd input_file (Some output_file) (Some fid)) with
        | Error (`Msg msg) -> Log.err "%s" msg
        | Ok () -> ()
      end;
      let data = Io.read_file output_file in
      let func = Parsing.parse_func data in
      Env.add_func env fid func;
      Choice.return (Ok (Value.Val (Val.Str fid)))
    in
    of_array [| ("parseJS", Extern_func (Func (Arg Res), parseJS)) |]

  let concrete_api =
    let open Extern_func in
    let int_to_four_hex _v = assert false in
    let octal_to_decimal _v = assert false in
    let to_precision _v1 _v2 = assert false in
    let to_exponential _v1 _v2 = assert false in
    let to_fixed _v1 _v2 = assert false in
    let from_char_code_u _v = assert false in
    let to_char_code_u _v = assert false in
    let to_lower_case _v = assert false in
    let to_upper_case _v = assert false in
    let trim _v = assert false in
    let s_len_u _v = assert false in
    let s_nth_u _v1 _v2 = assert false in
    let s_split _v1 _v2 = assert false in
    let s_substr_u _v1 _v2 _v3 = assert false in
    let array_len _v = assert false in
    let array_make _v1 _v2 = assert false in
    let array_nth _v1 _v2 = assert false in
    let array_set _v1 _v2 _v3 = assert false in
    let list_to_array _v = assert false in
    let list_sort _v = assert false in
    let list_mem _v1 _v2 = assert false in
    let list_remove_last _v = assert false in
    let list_remove _v1 _v2 = assert false in
    let list_remove_nth _v1 _v2 = assert false in
    let float_to_byte _v = assert false in
    let float32_to_le_bytes _v = assert false in
    let float32_to_be_bytes _v = assert false in
    let float64_to_le_bytes _v = assert false in
    let float64_to_be_bytes _v = assert false in
    let float32_from_le_bytes _v = assert false in
    let float32_from_be_bytes _v = assert false in
    let float64_from_le_bytes _v = assert false in
    let float64_from_be_bytes _v = assert false in
    let bytes_to_string _v = assert false in
    let int_to_be_bytes _v1 _v2 = assert false in
    let int_from_le_bytes _v1 _v2 = assert false in
    let uint_from_le_bytes _v1 _v2 = assert false in
    let log_2 _v = assert false in
    let log_e _v = assert false in
    let log_10 _v = assert false in
    let sin _v = assert false in
    let cos _v = assert false in
    let tan _v = assert false in
    let sinh _v = assert false in
    let cosh _v = assert false in
    let tanh _v = assert false in
    let asin _v = assert false in
    let acos _v = assert false in
    let atan _v = assert false in
    let atan2 _v1 _v2 = assert false in
    let utf8_decode _v = assert false in
    let hex_decode _v = assert false in
    let parse_number _v = assert false in
    let parse_string _v = assert false in
    let parse_date _v = assert false in
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
       ; ("list_to_array_external", Extern_func (Func (Arg Res), list_to_array))
       ; ("l_sort_external", Extern_func (Func (Arg Res), list_sort))
       ; ("in_list_external", Extern_func (Func (Arg (Arg Res)), list_mem))
       ; ( "l_remove_last_external"
         , Extern_func (Func (Arg Res), list_remove_last) )
       ; ("l_remove_external", Extern_func (Func (Arg (Arg Res)), list_remove))
       ; ( "l_remove_nth_external"
         , Extern_func (Func (Arg (Arg (Arg Res))), list_remove_nth) )
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
         (* parse *)
       ; ("utf8_decode_external", Extern_func (Func (Arg Res), utf8_decode))
       ; ("hex_decode_external", Extern_func (Func (Arg Res), hex_decode))
       ; ("parse_number_external", Extern_func (Func (Arg Res), parse_number))
       ; ("parse_string_external", Extern_func (Func (Arg Res), parse_string))
       ; ("parse_date_external", Extern_func (Func (Arg Res), parse_date))
      |]

  let symbolic_api =
    let open Value in
    let open Extern_func in
    let non_empty = function
      | Val (Str "") -> Val (Str (fresh_x ()))
      | Val (Str _) as x -> x
      | x -> Log.fail "'%a' is not a valid string symbol" Value.pp x
    in
    let str_symbol (x : value) =
      Choice.return (Ok (Symbolic (Type.StrType, non_empty x)))
    in
    let int_symbol (x : value) =
      Choice.return (Ok (Value.int_symbol (non_empty x)))
    in
    let flt_symbol (x : value) =
      Choice.return (Ok (Symbolic (Type.FltType, non_empty x)))
    in
    let bool_symbol (x : value) =
      Choice.return (Ok (Symbolic (Type.BoolType, non_empty x)))
    in
    let is_symbolic (n : value) =
      Choice.return (Ok (Val (Val.Bool (Value.is_symbolic n))))
    in
    let is_number (n : value) =
      let is_number =
        match Value_typing.type_of n with
        | Some Type.IntType | Some Type.FltType -> true
        | _ -> false
      in
      Choice.return (Ok (Val (Val.Bool is_number)))
    in
    let is_sat (e : value) =
      let/ b = Choice.check e in
      Choice.return (Ok (Val (Val.Bool b)))
    in
    let is_exec_sat (e : value) =
      (* TODO: more fine-grained exploit analysis *)
      let i = Value.int_symbol_s (fresh_i ()) in
      let len = Value.Val (Int 18) in
      let sub = TriOpt (Operator.StringSubstr, e, i, len) in
      let query =
        BinOpt (Operator.Eq, sub, Val (Val.Str "A; touch success #"))
      in
      let/ b = Choice.check_add_true query in
      Choice.return (Ok (Val (Val.Bool b)))
    in
    let is_eval_sat (e : value) =
      (* TODO: more fine-grained exploit analysis *)
      let i = Value.int_symbol_s (fresh_i ()) in
      let len = Value.Val (Int 25) in
      let sub = TriOpt (Operator.StringSubstr, e, i, len) in
      let query =
        BinOpt (Operator.Eq, sub, Val (Val.Str ";console.log('success')//"))
      in
      let/ b = Choice.check_add_true query in
      Choice.return (Ok (Val (Val.Bool b)))
    in
    let abort (e : value) =
      let e' = Format.asprintf "%a" Value.pp e in
      Log.out "      abort : %s@." e';
      Choice.return @@ Error (`Abort e')
    in
    let assume (e : value) thread =
      let e' = Translator.translate e in
      [ (Ok (Val (Val.Symbol "undefined")), Thread.add_pc thread e') ]
    in
    let evaluate (e : value) thread =
      let e' = Translator.translate e in
      let pc = Thread.pc thread |> PC.to_list in
      let solver = Thread.solver thread in
      assert (Solver.check solver (e' :: pc));
      let v = Solver.get_value solver e' in
      [ (Ok (Translator.expr_of_value v.node.e), thread) ]
    in
    let optimize target opt e pc =
      Optimizer.push opt;
      Optimizer.add opt pc;
      let v = target opt e in
      Optimizer.pop opt;
      v
    in
    let maximize (e : value) thread =
      let e' = Translator.translate e in
      let pc = Thread.pc thread |> PC.to_list in
      let opt = Thread.optimizer thread in
      let v = optimize Optimizer.maximize opt e' pc in
      match v with
      | Some v -> [ (Ok (Translator.expr_of_value (Val v)), thread) ]
      | None ->
        (* TODO: Error here *)
        assert false
    in
    let minimize (e : value) thread =
      let e' = Translator.translate e in
      let pc = Thread.pc thread |> PC.to_list in
      let opt = Thread.optimizer thread in
      let v = optimize Optimizer.minimize opt e' pc in
      match v with
      | Some v -> [ (Ok (Translator.expr_of_value (Val v)), thread) ]
      | None ->
        (* TODO: Error here *)
        assert false
    in
    let print (v : Value.value) =
      Log.out "extern print: %a@." Value.pp v;
      Choice.return (Ok (Value.Val (Val.Symbol "undefined")))
    in
    of_array
      [| ("str_symbol", Extern_func (Func (Arg Res), str_symbol))
       ; ("int_symbol", Extern_func (Func (Arg Res), int_symbol))
       ; ("flt_symbol", Extern_func (Func (Arg Res), flt_symbol))
       ; ("bool_symbol", Extern_func (Func (Arg Res), bool_symbol))
       ; ("is_symbolic", Extern_func (Func (Arg Res), is_symbolic))
       ; ("is_number", Extern_func (Func (Arg Res), is_number))
       ; ("is_sat", Extern_func (Func (Arg Res), is_sat))
       ; ("is_exec_sat", Extern_func (Func (Arg Res), is_exec_sat))
       ; ("is_eval_sat", Extern_func (Func (Arg Res), is_eval_sat))
       ; ("abort", Extern_func (Func (Arg Res), abort))
       ; ("assume", Extern_func (Func (Arg Res), assume))
       ; ("evaluate", Extern_func (Func (Arg Res), evaluate))
       ; ("maximize", Extern_func (Func (Arg Res), maximize))
       ; ("minimize", Extern_func (Func (Arg Res), minimize))
       ; ("value", Extern_func (Func (Arg Res), print))
      |]
end

include (Make () : S with type extern_func := Symbolic.P.Extern_func.extern_func)

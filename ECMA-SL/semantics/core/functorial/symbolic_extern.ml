open EslCore
open EslSyntax
module Env = Symbolic.P.Env
module SMap = Link_env.SMap
module PC = Choice_monad.PC

module type S = sig
  type extern_func

  val api : Env.t -> extern_func SMap.t
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

  let api env =
    let open Value in
    let open Extern_func in
    let non_empty = function
      | Val (Str "") -> Val (Str (fresh_x ()))
      | Val (Str _) as x -> x
      | x -> Log.err "'%a' is not a valid string symbol" Value.pp x
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
      Log.log ~header:false "      abort : %s" e';
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
    let print (v : Value.value) =
      Fmt.printf "extern print: %a@." Value.pp v;
      Choice.return (Ok (Value.Val (Val.Symbol "undefined")))
    in
    SMap.of_seq
      (Array.to_seq
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
          ; ("parseJS", Extern_func (Func (Arg Res), parseJS))
          ; ("value", Extern_func (Func (Arg Res), print))
         |] )
end

include (
  Make (struct end) :
    S with type extern_func := Symbolic.P.Extern_func.extern_func )

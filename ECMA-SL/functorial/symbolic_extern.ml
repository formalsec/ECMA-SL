module SMap = Link_env.SMap

module type S = sig
  type extern_func

  val api : extern_func SMap.t
end

module Make () = struct
  module Value = Symbolic.P.Value
  module Choice = Symbolic.P.Choice
  module Extern_func = Symbolic.P.Extern_func
  module Thread = Choice_monad.Thread
  module Translator = Value_translator
  module Optimizer = Choice_monad.Optimizer

  let ( let/ ) = Choice.bind
  let fresh_i = Utils.make_name_generator "i"
  let fresh_len = Utils.make_name_generator "len"

  let api =
    let open Value in
    let open Extern_func in
    let str_symbol (x : value) =
      Choice.return (Ok (Symbolic (Type.StrType, x)))
    in
    let int_symbol (x : value) = Choice.return (Ok (Value.int_symbol x)) in
    let flt_symbol (x : value) =
      Choice.return (Ok (Symbolic (Type.FltType, x)))
    in
    let bool_symbol (x : value) =
      Choice.return (Ok (Symbolic (Type.BoolType, x)))
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
      let len = Value.int_symbol_s (fresh_len ()) in
      let sub = TriOpt (Operator.StringSubstr, e, i, len) in
      let query =
        BinOpt (Operator.Eq, sub, Val (Val.Str "; touch success #"))
      in
      let/ b = Choice.check_add_true query in
      Choice.return (Ok (Val (Val.Bool b)))
    in
    let is_eval_sat (e : value) =
      (* TODO: more fine-grained exploit analysis *)
      let i = Value.int_symbol_s (fresh_i ()) in
      let len = Value.int_symbol_s (fresh_len ()) in
      let sub = TriOpt (Operator.StringSubstr, e, i, len) in
      let query =
        BinOpt (Operator.Eq, sub, Val (Val.Str ";console.log('success')//"))
      in
      let/ b = Choice.check_add_true query in
      Choice.return (Ok (Val (Val.Bool b)))
    in
    let abort (e : value) =
      let e' = Format.asprintf "%a" Value.pp e in
      Log.warn "      abort : %s@." e';
      Choice.return @@ Error (`Abort e')
    in
    let assume (e : value) thread =
      let e' = Translator.translate e in
      [ (Ok (Val (Val.Symbol "undefined")), Thread.add_pc thread e') ]
    in
    let evaluate (e : value) thread =
      let e' = Translator.translate e in
      let pc = Thread.pc thread in
      let solver = Thread.solver thread in
      assert (Solver.check solver (e' :: pc));
      let v = Solver.get_value solver e' in
      [ (Ok (Translator.expr_of_value v.e), thread) ]
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
      let pc = Thread.pc thread in
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
      let pc = Thread.pc thread in
      let opt = Thread.optimizer thread in
      let v = optimize Optimizer.minimize opt e' pc in
      match v with
      | Some v -> [ (Ok (Translator.expr_of_value (Val v)), thread) ]
      | None ->
        (* TODO: Error here *)
        assert false
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
         |] )
end

include (
  Make (struct end) :
    S with type extern_func := Symbolic.P.Extern_func.extern_func )

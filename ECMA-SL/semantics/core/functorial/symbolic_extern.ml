open EslBase
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
    let exec (e : value) thread =
      (* TODO: more fine-grained exploit analysis *)
      if not @@ Value.is_symbolic e then
        [ (Ok (Val (Val.Symbol "undefined")), thread) ]
      else
        let open Encoding.Expr in
        let v = Translator.translate e in
        let query =
          binop Ty_str Seq_contains v (make (Val (Str "`touch success`")))
        in
        Log.log ~header:false "       exec : %a" Value.pp e;
        [ (Error (`Exec_failure e), Thread.add_pc thread query) ]
    in
    let eval (e : value) thread =
      (* TODO: more fine-grained exploit analysis *)
      if not @@ Value.is_symbolic e then
        [ (Ok (Val (Val.Symbol "undefined")), thread) ]
      else
        let open Encoding.Expr in
        let v = Translator.translate e in
        let query =
          binop Ty_str Seq_contains v
            (make (Val (Str ";console.log('success')//")))
        in
        Log.log ~header:false "       eval : %a" Value.pp e;
        [ (Error (`Eval_failure e), Thread.add_pc thread query) ]
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
      assert (`Sat = Solver.check solver (e' :: pc));
      let v = Solver.get_value solver e' in
      [ (Ok (Translator.expr_of_value (Encoding.Expr.view v)), thread) ]
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
    let str_replace (s : Value.value) (t : Value.value) (t' : Value.value)
      thread =
      let open Encoding.Expr in
      let x = fresh_x () in
      let sym = Encoding.Symbol.(x @: Ty_str) in
      let s = Translator.translate s in
      let t = Translator.translate t in
      let t' = Translator.translate t' in
      let replace_str = triop Ty_str Seq_replace s t t' in
      let cond = relop Ty_bool Eq (mk_symbol sym) replace_str in
      [ (Ok (Symbolic (Type.StrType, Val (Str x))), Thread.add_pc thread cond) ]
    in
    let str_indexOf (s : Value.value) (t : Value.value) (_i : Value.value)
      thread =
      let open Encoding.Expr in
      let index0 = fresh_x () in
      let index1 = fresh_x () in
      let sym0 = mk_symbol Encoding.Symbol.(index0 @: Ty_int) in
      let sym1 = mk_symbol Encoding.Symbol.(index1 @: Ty_real) in
      let s = Translator.translate s in
      let t = Translator.translate t in
      (* let i = Translator.translate i in *)
      let i = make (Val (Int 0)) in
      let indexOf = triop Ty_str Seq_index s t i in
      let cond0 = relop Ty_bool Eq sym0 indexOf in
      let cond1 = relop Ty_bool Eq sym0 (cvtop Ty_int Reinterpret_float sym1) in
      [ ( Ok (Symbolic (Type.FltType, Val (Str index1)))
        , Thread.add_pc (Thread.add_pc thread cond0) cond1 )
      ]
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
          ; ("exec", Extern_func (Func (Arg Res), exec))
          ; ("eval", Extern_func (Func (Arg Res), eval))
          ; ("abort", Extern_func (Func (Arg Res), abort))
          ; ("assume", Extern_func (Func (Arg Res), assume))
          ; ("evaluate", Extern_func (Func (Arg Res), evaluate))
          ; ("maximize", Extern_func (Func (Arg Res), maximize))
          ; ("minimize", Extern_func (Func (Arg Res), minimize))
          ; ("parseJS", Extern_func (Func (Arg Res), parseJS))
          ; ("value", Extern_func (Func (Arg Res), print))
          ; ( "str_replace"
            , Extern_func (Func (Arg (Arg (Arg Res))), str_replace) )
          ; ( "str_indexOf"
            , Extern_func (Func (Arg (Arg (Arg Res))), str_indexOf) )
         |] )
end

include (
  Make () :
    S with type extern_func := Symbolic.P.Extern_func.extern_func )

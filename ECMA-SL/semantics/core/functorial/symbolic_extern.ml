open EslBase
open EslSyntax
module Env = Symbolic.P.Env
module SMap = Link_env.SMap
module PC = Choice_monad.PC

module type S = sig
  type extern_func

  val api : Fpath.t -> Env.t -> extern_func SMap.t
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

  let api filename env =
    let open Value in
    let open Extern_func in
    let non_empty = function
      | Val (Str "") -> Val (Str (fresh_x ()))
      | Val (Str _) as x -> x
      | x -> Log.err "'%a' is not a valid string symbol" Value.pp x
    in
    let str_symbol (x : value) =
      Choice.return @@ Ok (Symbolic (Type.StrType, non_empty x))
    in
    let int_symbol (x : value) =
      Choice.return @@ Ok (Value.int_symbol (non_empty x))
    in
    let flt_symbol (x : value) =
      Choice.return @@ Ok (Symbolic (Type.FltType, non_empty x))
    in
    let bool_symbol (x : value) =
      Choice.return @@ Ok (Symbolic (Type.BoolType, non_empty x))
    in
    let is_symbolic (n : value) =
      Choice.return @@ Ok (Val (Val.Bool (Value.is_symbolic n)))
    in
    let is_number (n : value) =
      let is_number =
        match Value_typing.type_of n with
        | Some Type.IntType | Some Type.FltType -> true
        | _ -> false
      in
      Choice.return @@ Ok (Val (Val.Bool is_number))
    in
    let is_sat (e : value) =
      let/ b = Choice.check e in
      Choice.return @@ Ok (Val (Val.Bool b))
    in
    let exec (e : value) =
      (* TODO: more fine-grained exploit analysis *)
      match e with
      | Val _ -> Choice.return @@ Ok (Val (Val.Symbol "undefined"))
      | _ ->
        (* TODO: Use with_state instead *)
        fun thread ->
         let open Smtml.Expr in
         let v = Translator.translate e in
         let query =
           binop Ty_str String_contains v (value (Str "`touch success`"))
         in
         Log.log ~header:false "       exec : %a" Value.pp e;
         [ (Error (`Exec_failure e), Thread.add_pc thread query) ]
    in
    let eval (e : value) =
      (* TODO: more fine-grained exploit analysis *)
      match e with
      | Val _ -> Choice.return @@ Ok (Val (Val.Symbol "undefined"))
      | _ ->
        (* TODO: Use with_state instead *)
        fun thread ->
         let open Smtml.Expr in
         let v = Translator.translate e in
         let query =
           binop Ty_str String_contains v
             (value (Str ";console.log('success')//"))
         in
         Log.log ~header:false "       eval : %a" Value.pp e;
         [ (Error (`Eval_failure e), Thread.add_pc thread query) ]
    in
    let readFile (e : value) =
      match e with
      | Val _ -> Choice.return @@ Ok (Val (Val.Symbol "undefined"))
      | _ ->
        (* TODO: Use with_state instead *)
        fun thread ->
         let open Smtml.Expr in
         let v = Translator.translate e in
         let query =
           binop Ty_str String_contains v (value (Str "./exploited"))
         in
         Log.log ~header:false "   readFile : %a" Value.pp e;
         [ (Error (`ReadFile_failure e), Thread.add_pc thread query) ]
    in
    let abort (e : value) =
      let e' = Format.asprintf "%a" Value.pp e in
      Log.log ~header:false "      abort : %s" e';
      Choice.return @@ Error (`Abort e')
    in
    let assume (e : value) =
      let open Smtml in
      let e' = Translator.translate e in
      match Expr.view e' with
      | Val Value.False -> Choice.empty
      | _ ->
        (* TODO: Use with_state instead *)
        fun thread ->
         [ (Ok (Val (Val.Symbol "undefined")), Thread.add_pc thread e') ]
    in
    let evaluate (e : value) =
      Choice.with_state (fun state ->
          let e = Translator.translate e in
          let pc = Thread.pc state |> PC.to_list in
          let solver = Thread.solver state in
          assert (`Sat = Solver.check solver (e :: pc));
          let v = Solver.get_value solver e in
          Ok (Translator.expr_of_value (Smtml.Expr.view v)) )
    in
    let optimize target opt e pc =
      Optimizer.push opt;
      Optimizer.add opt pc;
      let v = target opt e in
      Optimizer.pop opt;
      v
    in
    let maximize (e : value) =
      Choice.with_state (fun state ->
          let e' = Translator.translate e in
          let pc = Thread.pc state |> PC.to_list in
          let opt = Thread.optimizer state in
          let v = optimize Optimizer.maximize opt e' pc in
          match v with
          | Some v -> Ok (Translator.expr_of_value (Val v))
          | None ->
            (* TODO: Error here *)
            assert false )
    in
    let minimize (e : value) =
      Choice.with_state (fun thread ->
          let e' = Translator.translate e in
          let pc = Thread.pc thread |> PC.to_list in
          let opt = Thread.optimizer thread in
          let v = optimize Optimizer.minimize opt e' pc in
          match v with
          | Some v -> Ok (Translator.expr_of_value (Val v))
          | None ->
            (* TODO: Error here *)
            assert false )
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
      (* TODO: Use with_state here instead *)
      let open Smtml.Expr in
      let x = fresh_x () in
      let sym = Smtml.Symbol.(x @: Ty_str) in
      let s = Translator.translate s in
      let t = Translator.translate t in
      let t' = Translator.translate t' in
      let replace_str = triop Ty_str String_replace s t t' in
      let cond = relop Ty_bool Eq (mk_symbol sym) replace_str in
      [ (Ok (Symbolic (Type.StrType, Val (Str x))), Thread.add_pc thread cond) ]
    in
    let str_indexof (s : Value.value) (t : Value.value) (_i : Value.value)
      thread =
      (* TODO: Use with_state here instead *)
      let open Smtml.Expr in
      let index = fresh_x () in
      let sym = mk_symbol Smtml.Symbol.(index @: Ty_real) in
      let s = Translator.translate s in
      let t = Translator.translate t in
      (* let i = Translator.translate i in *)
      let i = make (Val (Int 0)) in
      let indexOf = triop Ty_str String_index s t i in
      let indexOf2real = cvtop Ty_real Reinterpret_int indexOf in
      let cond = relop Ty_bool Eq sym indexOf2real in
      [ ( Ok (Symbolic (Type.FltType, Val (Str index)))
        , Thread.add_pc thread cond )
      ]
    in
    let str_lastIndexOf (s : Value.value) (t : Value.value) thread =
      (* TODO: Use with_state here instead *)
      let open Smtml.Expr in
      let index = fresh_x () in
      let sym = mk_symbol Smtml.Symbol.(index @: Ty_real) in
      let s = Translator.translate s in
      let t = Translator.translate t in
      (* let i = Translator.translate i in *)
      let indexOf = binop Ty_str String_last_index s t in
      let indexOf2real = cvtop Ty_real Reinterpret_int indexOf in
      let cond = relop Ty_bool Eq sym indexOf2real in
      [ ( Ok (Symbolic (Type.FltType, Val (Str index)))
        , Thread.add_pc thread cond )
      ]
    in
    let str_sub (s : Value.value) (start : Value.value) (len : Value.value)
      thread =
      (* TODO: Use with_state here instead *)
      let open Smtml.Expr in
      let x = fresh_x () in
      let sym = mk_symbol Smtml.Symbol.(x @: Ty_str) in
      let s = Translator.translate s in
      let start = Translator.translate start in
      let len = Translator.translate len in
      let substr = triop Ty_str String_extract s start len in
      let cond = relop Ty_bool Eq sym substr in
      [ (Ok (Symbolic (Type.StrType, Val (Str x))), Thread.add_pc thread cond) ]
    in
    let str_parse_int (str : Value.value) thread =
      (* TODO: Use with_state here instead *)
      let open Smtml.Expr in
      let x = fresh_x () in
      let sym = mk_symbol Smtml.Symbol.(x @: Ty_real) in
      let str = Translator.translate str in
      let str2int = cvtop Ty_str String_to_int str in
      let int2real = cvtop Ty_real Reinterpret_int str2int in
      let cond = relop Ty_bool Eq sym int2real in
      [ (Ok (Symbolic (Type.FltType, Val (Str x))), Thread.add_pc thread cond) ]
    in
    let str_split (s : Value.value) (r : Value.value) thread =
      (* TODO: Use with_state here instead *)
      let x1 = fresh_x ()
      and x2 = fresh_x () in
      let cond =
        let open Smtml in
        let x1 = Expr.mk_symbol Symbol.(x1 @: Ty_str) in
        let x2 = Expr.mk_symbol Symbol.(x2 @: Ty_str) in
        let sep = Translator.translate r in
        let s = Translator.translate s in
        let s' = Expr.naryop Ty_str Concat [ x1; sep; x2 ] in
        Expr.relop Ty_bool Eq s s'
      in
      let result = Value.(mk_list [ str_symbol x1; str_symbol x2 ]) in
      [ (Ok result, Thread.add_pc thread cond) ]
    in
    let str_match (s : Value.value) thread =
      (* TODO: Use with_state here instead *)
      let x = fresh_x () in
      let cond =
        let open Smtml in
        let x = Expr.mk_symbol Symbol.(x @: Ty_str) in
        let s = Translator.translate s in
        Expr.binop Ty_str String_contains s x
      in
      let result = Value.(mk_list [ str_symbol x ]) in
      [ (Ok result, Thread.add_pc thread cond) ]
    in

    let dirname () =
      let (dirname, _) = Fpath.split_base filename in
      Choice.return (Ok (Val (Val.Str (Fpath.to_string dirname))))
    in
    let filename () =
      let filename = Fpath.to_string filename in
      Choice.return (Ok (Val (Val.Str filename)))
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
          ; ("readFile", Extern_func (Func (Arg Res), readFile))
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
            , Extern_func (Func (Arg (Arg (Arg Res))), str_indexof) )
          ; ( "str_lastIndexOf"
            , Extern_func (Func (Arg (Arg Res)), str_lastIndexOf) )
          ; ("str_sub", Extern_func (Func (Arg (Arg (Arg Res))), str_sub))
          ; ("str_parseInt", Extern_func (Func (Arg Res), str_parse_int))
          ; ("str_match", Extern_func (Func (Arg Res), str_match))
          ; ("__dirname", Extern_func (Func (UArg Res), dirname))
          ; ("__filename", Extern_func (Func (UArg Res), filename))
          ; ("summ_string_split", Extern_func (Func (Arg (Arg Res)), str_split))
         |] )
end

include (Make () : S with type extern_func := Symbolic.P.Extern_func.extern_func)

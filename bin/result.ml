open Ecma_sl

type 'a pp = 'a Fmt.t * 'a

type cmderr =
  [ `Compile of Compile_error.t pp
  | `Runtime of Runtime_error.t pp
  | `Typing
  | `Encode of string
  | `Execute of string
  | `Test
  | `TestFmt of string
  | `Symbolic of Symbolic_error.t
  | `Generic of string
  ]

type 'a t = ('a, cmderr) Stdlib.Result.t

let log_error (err : cmderr) : unit =
  match err with
  | `Compile (pp, msg) -> Log.stderr "%a@." pp msg
  | `Runtime (pp, msg) -> Log.stderr "%a@." pp msg
  | `Typing -> ()
  | `Encode msg -> Log.error "%s" msg
  | `Execute msg -> Log.error "%s" msg
  | `Test -> ()
  | `TestFmt msg -> Log.error "%s" msg
  | `Symbolic _ -> ()
  | `Generic msg -> Log.error "%s" msg

let error (err : cmderr) : 'a t =
  log_error err;
  Error err
[@@inline]

let bos (res : ('a, [< `Msg of string ]) result) : 'a t =
  match res with Ok res' -> Ok res' | Error (`Msg err) -> error (`Generic err)

let esl_exec (exec_f : unit -> 'a t) : 'a t =
  try exec_f () with
  | Compile_error.Error err -> error (`Compile (Compile_error.pp, err))
  | Runtime_error.Error err -> error (`Runtime (Runtime_error.pp, err))

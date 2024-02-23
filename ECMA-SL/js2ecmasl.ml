(* This module is temporary and should be removed as soon as the JS2ECMA-SL is rewritten in OCaml using the Flow parser *)
open Bos

let set_output (output : string option) (cmd : Cmd.t) : Cmd.t =
  match output with
  | None -> cmd
  | Some output' -> Cmd.(add_args cmd (v "-o" % output'))

let set_builder (builder : string option) (cmd : Cmd.t) : Cmd.t =
  match builder with
  | None -> cmd
  | Some builder' -> Cmd.(add_args cmd (v "-b" % builder'))

let cmd (input : string) (output : string option) (builder : string option) :
  Cmd.t =
  Cmd.(v "js2ecma-sl" % "-s" % "-c" % "-i" % input)
  |> set_output output
  |> set_builder builder

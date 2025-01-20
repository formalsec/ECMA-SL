open Smtml_prelude.Result

(** This command is similar to `test` but it's for symbolic execution *)
let run ~inputs =
  let* files = Files.generate_input_list inputs in
  Fmt.pr "@[<v>%a@]@."
    (Fmt.list ~sep:Fmt.cut (Fmt.pair ~sep:Fmt.comma Fpath.pp Fpath.pp))
    files;
  Ok ()

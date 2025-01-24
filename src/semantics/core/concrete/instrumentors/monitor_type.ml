type t =
  | Nop
  | Printer
  | Other

let pp fmt mon =
  match mon with
  | Nop -> Fmt.pf fmt "nop"
  | Printer -> Fmt.pf fmt "pp"
  | Other -> Fmt.pf fmt "other"

let of_string str =
  match String.lowercase_ascii str with
  | "nop" -> Ok Nop
  | "pp" | "printer" -> Ok Printer
  | "other" -> Ok Other
  | _ -> Error "Unknown monitor"

let conv =
  let parser str =
    match of_string str with Ok v -> `Ok v | Error err -> `Error err
  in
  (parser, pp)

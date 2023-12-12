type version =
  | Latest
  | ToyEcma
  | ECMARef5
  | ECMARef6

let latest = ("latest", Latest)
let toyecma = ("toyecma", ToyEcma)
let ecmaref5 = ("ecmaref5", ECMARef5)
let ecmaref6 = ("ecmaref6", ECMARef6)

type builder =
  | Never
  | IfMissing
  | Always

let never = ("never", Never)
let if_missing = ("if_missing", IfMissing)
let always = ("always", Always)

type t =
  { filename : Fpath.t
  ; mutable execution_time : float
  ; mutable solver_time : float
  ; mutable solver_queries : int
  ; mutable num_failures : int
  ; mutable failures : Symbolic_error.t list
  }

let to_json
  { filename
  ; execution_time
  ; solver_time
  ; solver_queries
  ; num_failures
  ; failures
  } =
  `Assoc
    [ ("filename", `String (Fpath.to_string filename))
    ; ("execution_time", `Float execution_time)
    ; ("solver_time", `Float solver_time)
    ; ("solver_queries", `Int solver_queries)
    ; ("num_failures", `Int num_failures)
    ; ("failures", `List (List.map Symbolic_error.to_json failures))
    ]

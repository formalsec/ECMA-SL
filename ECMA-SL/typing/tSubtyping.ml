open EType

let value_err (congruency : bool) (tref : EType.t) (tsrc : EType.t) : 'a =
  let make_err msg = Eslerr.(typing ~src:(ErrSrc.at tsrc) msg) in
  if congruency then make_err (BadCongruency (tref, tsrc))
  else make_err (BadSubtyping (tref, tsrc))

let type_check ?(congruency : bool = false) (tref : t) (tsrc : t) : unit =
  match (congruency, tref.it, tsrc.it) with
  | (_, _, _) when EType.equal tref tsrc -> ()
  | (_, _, AnyType) -> ()
  | (_, AnyType, _) -> ()
  | (false, UnknownType, _) -> ()
  | (false, _, NeverType) -> ()
  | (false, IntType, LiteralType (IntegerLit _)) -> ()
  | (false, FloatType, LiteralType (FloatLit _)) -> ()
  | (false, StringType, LiteralType (StringLit _)) -> ()
  | (false, BooleanType, LiteralType (BooleanLit _)) -> ()
  | (false, SymbolType, LiteralType (SymbolLit _)) -> ()
  | (_, _, _) -> value_err congruency tref tsrc

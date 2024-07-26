# Unreleased Versions

### Added

- The `skip` flag for Test262 tests to allow tests to be skipped.
- Line comments to ECMA-SL and Core ECMA-SL language.

### Changed

- Operators `exp`, `random`, and tuple related operators were removed from syntax and became external functions.
- The values used by ECMA-SL (`Val.ml`) were changed to the values of `Smtml`. In the next code snippet, you can see how it was converted:
```ml
type t =
  | Void                      (* -> App "void" [] *)
  | Null                      (* -> App "null" [] *)
  | Int of int                (* -> Int *)
  | Flt of (float[@unboxed])  (* -> Real *)
  | Str of string             (* -> Str *)
  | Bool of bool              (* -> True | False *)
  | Symbol of string          (* -> App "symbol" [Str s] *)
  | Loc of Loc.t              (* -> App "loc" [Int l] *)
  | Byte of int               (* Int *)
  | List of t list            (* -> List l *)
  | Arr of t array            (* removed from language *)
  | Tuple of t list           (* removed from language *)
  | Type of Type.t            (* removed from language *)
  | Curry of string * t list  (* -> App fn fvs *)
```

- Bitwise operators are now defined over integers instead of floats.
- The semicolon `;` usage was changed in the `.esl` syntax:
  - Imports/typedefs/simple statements terminate a semicolon;
  - Functions/compound statements don't terminate with a semicolon;
  - A lonely semicolon results in the `skip` statement.
- The `elif` keyword was removed from the `.esl` syntax.
  - It can now be fully replaced by the `else if` construct.
- The `sdefault` keyword was replaced by the `default` keyword.
  - This keyword is now shared by both the match-with and switch statements.
- The Explode-js commands and share files were removed.
- Math/string/list operators were removed from the syntax and became external functions.
- Symbolic expressions were removed from the language.

### Fixed

- Source regions for string values are now properly calculated.
- The conditional operator now only evaluates one of the branches in the concrete interpreter.

<br>
<br>
<br>

# v1.0.0

### Added

- Starts changelog in `CHANGES.md` (@filipeom)

## Unreleased

### Added

- `skip` flag for Test262 tests to allow tests to be skipped

### Changed

- Operators `exp`, `random`, `typeof` and tuple related operators were removed from syntax and became external functions. 
- The values used by ECMA-SL (`Val.ml`) were changed to the values of `Smtml`. In the next code snippet, you can see how it was converted:
```ocaml
type t =
  | Null (* -> App "null" [] *)
  | Void (* -> App "void" [] *)
  | Int of int (* -> Int *)
  | Flt of (float[@unboxed]) (* -> Real *)
  | Str of string  (* -> Str *)
  | Bool of bool  (* -> True | False *)
  | Symbol of string (* -> App "symbol" [Str s] *)
  | Loc of Lo. c.t (* -> App "loc" [Int l] *)
  | Arr of t array (* int (pointer to array) *)
  | List of t list (* -> List l *)
  | Tuple of t list (* -> List l *)
  | Byte of int (* Int *)
  | Type of Type.t  
    (* -> App t [] *) 
    (* t := "NullType" | "IntType"    | "RealType" | "StrType" 
          | "BoolType" | "SymbolType" | "LocType"  | "ListType" 
          | "TupleType"| "CurryType" *)
  | Curry of string * t list  (* -> App fn fvs *)
```
- Bitwise operators defined over integers.

### Fixed

## v1.0.0

### Added

- Starts changelog in `CHANGES.md` (@filipeom)

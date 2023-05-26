<h1 align="center">Typed ECMA-SL Documentation</h1>

<p align="center">
  <a href="#features">Features</a> •
  <a href="#conventions">Conventions</a> •
  <a href="#implementation">Implementation</a>
</p>

<br>
<br>



# Features

## Available Types

- **AnyType** $\rightarrow$ can hold values of any type and bypass type checking.
- **UnknownType** $\rightarrow$  represents values whose type is not known at compile time
- **NeverType** $\rightarrow$ represents a value that never occurs
- **UndefinedType** $\rightarrow$ represents the absence of a value
- **NullType** $\rightarrow$ represents the absence of an object value
- **NumberType** $\rightarrow$ represents numeric values, including integers and floating-point numbers
- **StringType** $\rightarrow$ represents textual data
- **BooleanType** $\rightarrow$ represents a logical value of either true or false
- **SymbolType** $\rightarrow$ represents unique and immutable identifiers
- **LiteralType** $\rightarrow$ represents a specific value
- **ListType** $\rightarrow$ represents a list of elements of a single type
- **TupleType** $\rightarrow$ represents a tuple of elements
- **UnionType** $\rightarrow$ represents a combination between multiple types
- **ObjectType** $\rightarrow$ represents an object type
- **RuntimeType** $\rightarrow$ represents a type used at runtime

<br>
<br>



# Conventions

## Variable Naming

- ================================================================================
- `f / r` $\rightarrow$ first element of a list / remaining elements of the list
- ================================================================================
- `e/expr: E_Expr.t` $\rightarrow$ expression
- `s/stmt : E_Stmt.t` $\rightarrow$ statement
- `f/func: E_Func.t` $\rightarrow$ function
- ================================================================================
- `t: E_Type.t` $\rightarrow$ type
- `v: Val.t` $\rightarrow$ value
- `x/var: string` $\rightarrow$ variable name
- `c: Operators.const` $\rightarrow$ constant
- `op: Operators.t` $\rightarrow$ operator
- `fexpr: E_Expr.t` $\rightarrow$ function expression
- `fname: string` $\rightarrow$ function name
- `param: string` $\rightarrow$ function parameter
- `arg: E_Expr.t` $\rightarrow$ function argument
- `ret: E_Expr.t` $\rightarrow$ function return
- `oe: E_Expr.t` $\rightarrow$ object expression
- `fe: E_Expr.t` $\rightarrow$ field expression
- ================================================================================
- `tobj: E_Type.tobj_t` $\rightarrow$ object type
- `tfld : E_Type.tfld_t` $\rightarrow$ field type (`nfld` for named field and `sfld` for summary field)
- `fn: string / ft: E_Type.t / fp: E_Type.tpres_t` $\rightarrow$  field name / field type / field presence
- ================================================================================
- `tctx: T_Ctx.t` $\rightarrow$ typing context
- `tvar: T_Ctx.tvar_t` $\rightarrow$ typing context variable
- `rt: E_Type.t / nt: E_Type.t / mt: bool` $\rightarrow$ reference type / narrowed typed / mutable type flag
- ================================================================================
- `tref: E_Type.t` $\rightarrow$ expected reference type
- `texpr: E_Type.t` $\rightarrow$ provided expression type
- ================================================================================

**Prefix / Suffix naming conventions**

- A `t` prefix represents the entity (e.g., `targ` for the type of an argument)
- A `r / n` prefix before a `t` represents a reference / narrow type
- A `s` suffix multiplicity (lists) (e.g., `match t with UnionType ts -> ...`) 
- An `f / r` suffix might be used to represent the first element of a list / rest of the list

<br>
<br>



# Implementation

<br>
<br>

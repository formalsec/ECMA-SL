<h1 align="center">Typed ECMA-SL Documentation</h1>

<p align="center">
  <a href="#features">Features</a> •
  <a href="#conventions">Conventions</a> •
  <a href="#definitions">Definitions</a> •
  <a href="#inference">Inference</a> •
  <a href="#implementation">Implementation</a>
</p>

<br>
<br>



# Features

## Available Types

The following itemize briefly describes every type available in Typed ECMA-SL.

- **AnyType** $\rightarrow$ can hold values of any type and bypass type checking.
- **UnknownType** $\rightarrow$  represents values whose type is not known at compile time
- **NeverType** $\rightarrow$ represents a value that never occurs
- **UndefinedType** $\rightarrow$ represents the absence of a value
- **NullType** $\rightarrow$ represents the absence of an object value
- **IntegerType** $\rightarrow$ represents integer numeric values
- **FloatType** $\rightarrow$ represents floating-point numeric values
- **StringType** $\rightarrow$ represents textual data
- **BooleanType** $\rightarrow$ represents a logical value of either true or false
- **SymbolType** $\rightarrow$ represents unique and immutable identifiers
- **LiteralType** $\rightarrow$ represents a specific value
- **ListType** $\rightarrow$ represents a list of elements of a single type
- **TupleType** $\rightarrow$ represents a tuple of elements
- **UnionType** $\rightarrow$ represents a combination between multiple types
- **SigmaType** $\rightarrow$ represents a tagged union of objects
- **ObjectType** $\rightarrow$ represents an object type
- **RuntimeType** $\rightarrow$ represents a type used at runtime

<br>

## User Defined Types
By using the keyword `typedef` it is possible to define an alias for a type. This mechanism also enables the possibility of defining recursive types. Below, we describe a possibility for the definition of the type of a linked list node. <br>
<center><code>typedef linkedList_t := { value: any, next: linkedList_t } | null </code></center>

<br>
<br>
<br>
<br>
<br>



# Conventions

## Variable Naming

- ================================================================================
- `tobj: E_Type.tobj_t` $\rightarrow$ object type
- `tfld : E_Type.tfld_t` $\rightarrow$ field type (`nfld` for named field and `sfld` for summary field)
- `fn: string / ft: E_Type.t / fp: E_Type.tpres_t` $\rightarrow$  field name / field type / field presence
- ================================================================================
- `e/expr: E_Expr.t` $\rightarrow$ expression
- `s/stmt : E_Stmt.t` $\rightarrow$ statement
- `f/func: E_Func.t` $\rightarrow$ function
- `p/pat: E_Pat.t` $\rightarrow$ pattern
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
- `d: string` $\rightarrow$ sigma type discriminant
- ================================================================================
- `tctx: T_Ctx.t` $\rightarrow$ typing context
- `tvar: T_Ctx.tvar_t` $\rightarrow$ typing context variable
- `rt: E_Type.t / nt: E_Type.t / mt: bool` $\rightarrow$ reference type / narrowed typed / mutable type flag
- ================================================================================
- `tref: E_Type.t` $\rightarrow$ expected reference type
- `texpr: E_Type.t` $\rightarrow$ provided expression type
- ================================================================================
- `sigmaModel: sigmaModel_t` $\rightarrow$ model of the sigma type for pattern-matching
- `sigmaObject: \sigmaObject_t` $\rightarrow$ object instance of the sigma type for pattern-matching
- `sigmaCase: sigmaCase_t` $\rightarrow$ unfolded case of the sigma object for pattern-matching
- `patValFld: patValFld_t` $\rightarrow$ processed pattern value/none field
- `patVarFld: patVarFld_t` $\rightarrow$ processed pattern var field
- `patFld: (string * E_Pat_v.t)` $\rightarrow$ pattern field of a pattern-matching case
- `pn: string / pv: E_Pat_v.t` $\rightarrow$ pattern field name / pattern field value
- `dv: E_Pat_v.t / dt: E_Type.t` $\rightarrow$ discriminant value / discriminant type
- ================================================================================

**Prefix / Suffix naming conventions**

- A `t` prefix represents the entity (e.g., `targ` for the type of an argument)
- A `r / n` prefix before a `t` represents a reference / narrow type
- A `s` suffix multiplicity (lists) (e.g., `match t with UnionType ts -> ...`) 
- An `f / r` suffix might be used to represent the first element of a list / rest of the list

<br>
<br>
<br>
<br>
<br>



# Inference

## Definitions

A **typing environment** is a map between variable names and variable types.

- **Type Environment** $(\Gamma) \Coloneqq x \mapsto \phi$ where:
  - $\phi \rightarrow$ variable type
- **Variable Type** $(\phi) \Coloneqq \{rt, nt, mt\}$ where:
  - $rt \rightarrow$ reference type (type annotation of the variable)
  - $nt \rightarrow$ narrowed type (guaranteed subset of the reference type)
  - $mt \rightarrow$ mutable type (flag that specifies whether the variable can change type)

The **narrowed** flag $(\omega)$ determines if the result of typing an expression should be narrowed or not.


<br>

---
## Expression Typing Rules

<center>

**Value:** $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
LiteralType(v) = \tau
\\\hline \Gamma, \_ \vdash v : \tau
\end{array}$

**Reference Variable**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\omega = false \hspace{2em} \Gamma(x) = \phi \hspace{2em} \phi^{rt} = \tau
\\\hline \Gamma, \omega \vdash, x : \tau
\end{array}$

**Narrowed Variable**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\omega = true \hspace{2em} \Gamma(x) = \phi \hspace{2em} \phi^{nt} = \tau
\\\hline \Gamma, \omega \vdash, x : \tau
\end{array}$

**Constant:** $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
ConstantType(c) = \tau
\\\hline \Gamma, \_ \vdash c : \tau
\end{array}$

**Unary Operators**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\Gamma, false \vdash e : \tau_e \hspace{2em} \oplus(\tau_e^{nt}) = \tau
\\\hline \Gamma, \_ \vdash \oplus(e) : \tau
\end{array}$

**Binary Operators**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\Gamma, false \vdash e_1 : \tau_{e_1} \hspace{2em} \Gamma, false \vdash e_2 : \tau_{e_2} \hspace{2em} \otimes(\tau_{e_1}^{nt}, \tau_{e_2}^{nt}) = \tau
\\\hline \Gamma, \_ \vdash \otimes(e_1, e_2) : \tau
\end{array}$

**Ternary Operators**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\Gamma, false \vdash e_1 : \tau_{e_1} \hspace{2em} \Gamma, false \vdash e_2 : \tau_{e_2} \hspace{2em} \Gamma, false \vdash e_3 : \tau_{e_3} \hspace{2em} \otimes(\tau_{e_1}^{nt}, \tau_{e_2}^{nt}, \tau_{e_3}^{nt}) = \tau
\\\hline \Gamma, \_ \vdash \otimes(e_1, e_2, e_3) : \tau
\end{array}$

**Function Call**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\Gamma, false \vdash e_i : \tau_{e_i} |_{i=1}^n \hspace{2em} \Gamma(f) = (\tau_{e_1}, ..., \tau_{e_n}) \mapsto \tau \hspace{2em}
\\\hline \Gamma, \_ \vdash f(e |_{i=1}^n) : \tau
\end{array}$

**New Object**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\Gamma, false \vdash e_i: \tau_{e_i} |_{i=1}^n \hspace{2em} \{\tau_{e_1}, ..., \tau_{e_n}\} = \tau
\\\hline \Gamma, \_ \vdash \{f_i: e_i |_{i=1}^n\} : \tau
\end{array}$

**Field Lookup (Object)**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\Gamma, \omega \vdash e : \{f_i: \tau_i |_{i=1}^k, f: \tau\}
\\\hline \Gamma, \omega \vdash e.f : \tau
\end{array}$

**Field Lookup (Union)**: $\hspace{2em}$
$\def\arraystretch{1.5}\begin{array}{c}
\Gamma, \omega \vdash e : \Sigma[\sigma_1, ..., \sigma_n] \hspace{2em} \Gamma, \omega \vdash \sigma_i : \tau_i |_{i=1}^n \hspace{2em}  \Sigma[\tau_1, ...,\tau_n] = \tau
\\\hline \Gamma, \omega \vdash e.f : \tau
\end{array}$

</center>

<br>
<br>
<br>
<br>
<br>

# Implementation

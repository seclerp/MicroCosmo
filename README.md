# :sparkles: MiniCosmo
<p align="center">
MiniCosmo is tiny language made for educational purposes. It compiles into CIL and can run under CLR platform.
</p>

---

This repo contains:
- Backend (compiler) for CIL
- Intermediate representation interpreter
- Specification and basic documentation of a language

## MicroCosmo 1 specification

MicroCosmo is a **strongly** **static** typed programming language.

Memory management done by GC.

### Data types

MicroCosmo supports builtin types:
- string
- int
- bool
- double

`string` - UTF-8 immutable container of symbols
`int` - 32-bit integer type
`bool` - Boolean value
`double` - 64-bit double-precision floating point type
`[type] array` - array of some type

### Variable declaration

Variable declaration looks like this:

`let variableName : variableType = someExpression`

Last part is optional and can be omitted:

`let variableName : variableType`

Variable **always must have type declaration** in MicroCosmo.

### Literals

Builtin types may be instantiated using literals:

string: `"str"`, `'str2'`

int: `1`, `234`, `32123`

### Builtin operators

MicroCosmo supports several operators on different variable types.

#### Numeric operators

`+` - binary sum operator

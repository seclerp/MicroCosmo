# :sparkles: MicroCosmo
<p align="center">
MicroCosmo is tiny language made for educational purposes. It compiles into CIL and can run under CLR platform.
</p>

---

This repo contains:
- Backend (compiler) for CIL
- Intermediate representation interpreter
- Specification and basic documentation of a language

## MicroCosmo 1 specification

MicroCosmo is a **strongly**, **static** typed programming language.

Memory management done by CLR's GC.

### Comments

MicroCosmo allows only single line comments using `#` symbol:

`# Single line comment`

### Data types

MicroCosmo supports builtin types:

- `string` - UTF-8 immutable container of symbols
- `int` - 32-bit integer type
- `bool` - Boolean value
- `double` - 64-bit double-precision floating point type
- `any` - special type, can contain any other type
- `[type] array` - array of some type

### Literals

Builtin types may be instantiated using literals:

- **string:** `"str"`, `'str2'`
- **int:** `1`, `234`, `32123`
- **bool:** `true`, `false`
- **double:** `1.0`, `0.234`, `.23`

### Variable declaration

Variable declaration looks like:

`let variableName : variableType`

### Variables scope

Scope of variables is defined inside block statements (`{ }`)

Each `if`, `else`, `while`, `func` declaration declares also a inner variable scope.

Main variable scope is called "global scope"

### Builtin operators

MicroCosmo supports several operators on different variable types.

#### Operators of any type
- `[expression] is [expression]` - equals
- `[expression] is not [expression]` - not equals

#### Numeric only operators
`number` - `double` or `int`

**Binary:**
- `[number] + [number]` - sum
- `[number] - [number]` - difference
- `[number] * [number]` - multiplication
- `[number] / [number]` - division 
- `[number] % [number]` - remainder of the division 
- `[number] ** [number]` - erection to degree 
- `[number] += [number]` - sum and assign new value
- `[number] -= [number]` - difference and assign new value
- `[number] *= [number]` - multiplication and assign new value
- `[number] /= [number]` - division and assign new value
- `[number] %= [number]` - remainder of the division and assign new value
- `[number] **= [number]` - erection to degree and assign new value
- `[number] > [number]` - bigger than
- `[number] < [number]` - less than
- `[number] >= [number]` - bigger than or equals
- `[number] <= [number]` - less than or equals

**Unary:**
- `+ [number]` - keep sign
- `- [number]` - change sign

#### Bool only operators

**Binary:**
- `[bool] or [bool]` - logical "OR"
- `[bool] and [bool]` - logical "AND"

**Unary:**
- `not [bool]` - logical "NOT"

### Built-in functions

- `print(a : string)` - prints given string value into screen
- `println(a : string)` - prints given string value into screen and adds newline character
- `readstr() : string` - gets string from input
- `readint() : int` - reads next integer number 
- `readreal() : double` - reads next double number 

### Control flow

MicroCosmo allows to control execution flow using `if .. else ..` expression statement:

```
let someBool : bool
someBool = true
let value : int
if (someBool) {
  value = 5
} else {
  value = 3
}
```

Else block is optional:

```
let a : int
a = readint()
let isFive : bool = a is 5
if (isFive) {
  println("4 equals 5")
}
```

### Loops

MicroCosmo supports only 1 type of loops - `while`:

```
let i : int
i = 0
while (i < 5) {
  printLn(i to string)
  i++
}
```

### User defined functions

User can define own functions. All functions have global scope. Functions cannot be defined inside another functions.

If function does not have return value, returnType is not specified.

Functions can return control at any time.
```
func name(arg1 : type, arg2 : type) : returnType {
  # Function body
}
```

Example:
```
# Returns true if integer is positive, otherwise false
func sign(value : int) : bool {
  return value > 0
}
```
### Program structure

Programs written in MicroCosmo must have function `main` with signature:
`func main()`

Main function is the entry point for every program.

Example:
```
func main() {
  # ...
}
```


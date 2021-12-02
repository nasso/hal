# hal

A Scheme interpreter in Haskell.

## Features

- Macro definitions with `define-syntax` and `syntax-rules`
- A bare bones [standard library](lang/base.scm)
- Syntax error messages with line and column numbers
- Continuations with `call/cc`
- Multiple values with `values`
- Exact integers, rationals and floating point numbers
- REPL with multi-line support

### Builtins and standard library

| Name               | Kind      |
| ------------------ | --------- |
| `-`                | procedure |
| `*`                | procedure |
| `/`                | procedure |
| `+`                | procedure |
| `<`                | procedure |
| `<=`               | procedure |
| `=`                | procedure |
| `>`                | procedure |
| `>=`               | procedure |
| `abs`              | procedure |
| `and`              | syntax    |
| `append`           | procedure |
| `apply`            | procedure |
| `atom?`            | procedure |
| `begin`            | syntax    |
| `boolean?`         | procedure |
| `call-with-values` | procedure |
| `call/cc`          | procedure |
| `car` .. `caaaar`  | procedure |
| `case`             | syntax    |
| `cdr` .. `cddddr`  | procedure |
| `char?`            | procedure |
| `complex?`         | procedure |
| `cond`             | syntax    |
| `cons*`            | procedure |
| `cons`             | procedure |
| `define-syntax`    | syntax    |
| `define`           | syntax    |
| `display`          | procedure |
| `div`              | procedure |
| `dump-heap`        | procedure |
| `eq?`              | procedure |
| `equal?`\*         | procedure |
| `eqv?`\*           | procedure |
| `error`\*          | procedure |
| `even?`            | procedure |
| `exact?`           | procedure |
| `expand`           | procedure |
| `filter`           | procedure |
| `fold-left`        | procedure |
| `fold-right`       | procedure |
| `if`               | syntax    |
| `inexact?`         | procedure |
| `lambda`           | syntax    |
| `let*`             | syntax    |
| `let`              | syntax    |
| `letrec*`          | syntax    |
| `letrec`\*         | syntax    |
| `list`             | procedure |
| `map`              | procedure |
| `max`              | procedure |
| `min`              | procedure |
| `mod`              | procedure |
| `negative?`        | procedure |
| `newline`          | procedure |
| `not`              | procedure |
| `null?`            | procedure |
| `number?`          | procedure |
| `odd?`             | procedure |
| `or`               | syntax    |
| `pair?`            | procedure |
| `positive?`        | procedure |
| `quote`            | syntax    |
| `rational?`        | procedure |
| `real?`            | procedure |
| `set!`             | syntax    |
| `string?`          | procedure |
| `symbol?`          | procedure |
| `syntax-rules`\*   | syntax    |
| `values`           | procedure |
| `void`             | procedure |
| `zero?`            | procedure |

\*implementation not fully compliant with the [r6rs] standard.

## Installation

Make sure [stack] is installed, then run:

```sh
stack install hal
```

This will generate a `hal` executable in the current directory.

## Usage

```txt
hal [FILES...] [-i]
```

- `FILES`: files to run
- `-i`: interactive mode (implied when no files are given)

The global environment is initialized with the standard library before running
the first file and is not cleared after running each file. That means all files
are run as if they were one single top-level program. After running all files,
the value of the last expression of the last file is displayed.

## Examples

See the [examples](examples) directory.

[r6rs]: http://www.r6rs.org/
[stack]: https://docs.haskellstack.org/en/stable/README/

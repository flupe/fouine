# parser & interpreter for the fouine programming language

## notes
- basic atomic types: `int`, `bool`, `unit`, `'a ref`, `fun` are supported.
- we can have references of any of the previous atomic types.
- exceptions are allowed.
- the `:=` operator returns `unit` to stay consistent with OCaml.


## parsing

### currently supported syntax
- a delimiter `;;` is **required** at the end of a given input.
- positive integer constants.
- boolean constants: `true` and `false`.
- integer arithmetic: `*`, `+`, `-`.
- integer comparisons: `<`, `>`, `<=`, `>=`, `=`, `<>`.
- boolean operations: `&&`, `||` and `not`.
- named function declaration with multiple arguments with the `let` syntax.

  ```ocaml
  let f x y =
    x + 2 * y
  ```
- anonymous function declaration with multiple arguments with the `fun` syntax.

  ```ocaml
  fun x y ->
    x + 2 * y
  ```
- `if`, `then`, `else` statement.
- exception handling syntax: `try ... with E x -> ...`.
- `prInt` command.
- reference syntax: `ref 2`, `!x`, `x := 3`.
- chained function calls, even with anonymous functions.
- scopes delimited by parens or `begin`, `end`.

### todo
- parse negative integers.
- follow the OCaml spec and allow for global `let` definitions in the outer scope.
- better error handling when a syntax error occurs.


## interpretation

### implemented already
- all arithmetical and boolean operations.
- control flow (`if`).
- variable definition and fetching through environments.
- closures and variable propagation.
- function calls, currying.
- full reference support.
- valid print operation.
- recursion.
- exceptions handling (with continuation).

### todo
- global refactor

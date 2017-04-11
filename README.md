# Parser & interpreter for the fouine programming language

## Notes
- basic atomic types: `int`, `bool`, `unit`, `'a ref`, `fun` are supported.
- we can have references of any of the previous atomic types.
- exceptions are allowed.
- the `:=` operator returns `unit` to stay consistent with OCaml.
- **dependency**: for now, to replicate a REPL-like feel, we use `rlwrap`.


## Parsing

### Currently supported syntax
- a delimiter `;;` is **required** at the end of a given input.
- any integer constants (including negative numbers).
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
- sequencing with `;`.
- `int array` support with `aMake` and the usual `arr.(i)` syntax.
- chained function calls, even with anonymous functions.
- scopes delimited by parens or `begin`, `end`.

### Todo
- follow the OCaml spec and allow for global `let` definitions in the outer scope.
- better error handling when a syntax error occurs.


## interpretation

### implemented already
- all arithmetical and boolean operations.
- control flow (`if`, `else`).
  Note that, as in OCaml, a `else` branch is optional in which case a `unit` value is implied.
- variable definition and fetching through environments.
- closures and variable propagation.
- function calls, currying.
- full reference support.
- valid print operation.
- recursion.
- exceptions handling (with continuation).

### todo
- global refactor (maybe not).
- add several let without `in` keyword.

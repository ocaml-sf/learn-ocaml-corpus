#### Reminder: *bool* expressions

Boolean expressions can be:

- Boolean constants `true` and `false`.

- Comparison expressions: `e1 cmp e2` where *cmp* is one of the operators
  `=` , `<>`, `<`, `<=` , `>` ou `>=`, and *e1* and *e2* are of the same
  type.
- Expressions constructed with logical operators:
  - `e1 && e2` (and)
  - `e1 || e2` (or)
  - `not e` (negation of)
  where *e1*, *e2* and *e* of type bool.

Beware of the `==` and `!=` comparisons: they should **never** be used in
OCaml unless you know exactly what you are doing (they perform *physical*
or *pointer* comparisons. For example, `(1,2)==(1,2)` will yield `false`).

Functions cannot be compared directly, but integers, characters, lists,
tuples, etc., can be compared. For numeric types, the usual order on
numbers is used; for characters, alphabetical order is used; for strings,
lexicographic order is used; for tuples, lexicographic order is used based
on their components, and so on.

** Question 1.** In the following code, replace `false` with a boolean
expression that evaluates to `true` if and only if the number `x` is within
(inclusive) the range from `0` to `10`.


```ocaml
let intervalle10 x = false
```

**Question 2.** In the following declarations, replace the boolean
expression with its value (`true` or `false`)

```ocaml
let valeur1 = true && false

let valeur2 = true || false

let valeur3 = not (true || true) && true
```

**Question 3.** In the following declarations, replace the expression on the
right-hand side with a simpler equivalent expression.

```ocaml
let simplifier1 x y = (x || y) || x

let simplifier2 x y = (x > 5 && x >= 7)

let simplifier3 x y z = x = y && y = z && x = z

let simplifier4 x y = x > 7 || (x <= 7 && y > 2)

let simplifier5 x = (((x = true) = false) = false) = true
```

**Question 4.** A leap year is a year whose number is divisible by 4, except if it is
also divisible by 100... unless it is also divisible by 400. In the following declaration,
replace `false` with a boolean expression that evaluates to `true` only when `x` is an
integer corresponding to a leap year.

```ocaml
let bissextile x = false
```

It is worth noting that the `mod` operator calculates the remainder of the division operation.
Specifically, `x mod y` is equal to `0` when `x` is divisible by `y`.

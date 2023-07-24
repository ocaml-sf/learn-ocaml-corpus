## Reminder: Conditional expressions

If `c` is an expression of type `bool` and `e1` and `e2` are expressions of the
same type, then `if c then e1 else e2` is an expression of the same type as `e1`
and `e2`.

**Question 1.**

For each of the following expressions, give its value, or `Error` if it doesn't
make sense.

```ocaml
let phrase1 = if false then 3 else 4

let phrase2 = if true then 5 else 6.

let phrase3 = if 3 + 4 then true else false
```

**Question 2.**

A conditional expression allows us to construct an expression using other expressions.
These other expressions can themselves be constructed using a conditional expression,
and so on (we say they are _nested_).

Replace the following conditional expressions with equivalent expressions containing
strictly fewer `if` statements.

```ocaml
let simplify1 x = if x > 3 then false else true

let simplify2 x y=
    if x then
        if y then false
        else true
    else
        if y then true
        else false

let edt day time =
    if day = "monday"
    then if 13 * 60 + 30 <= time && time < 15 * 60 + 30 then "practical"
         else "Nothing interesting"
    else if day = "thursday"
         then if 8 * 60 + 30 <= time && time < 10 * 60 + 30 then "lecture-tutorial"
              else "Nothing interesting"
         else "Nothing interesting"
```

**Question 3 ($$).**

In the following declaration, replace `"Approximately ..."` with an expression that approximately
expresses in days, hours, minutes, or seconds duration `t` expressed in seconds. For example, the
expression should evaluate to `"Approximately 3 hours"` if `t` is `9000`, or `"Approximately 2 days"`
if `t` is `180000`. For negative numbers, the result is not important.

_Note_: You can use the `string_of_int` function to convert an integer to a string.

```ocaml
let approximately t = "Approximately …"
```


**Bonus ($$$)**

Same question, but correctly adjust the words "seconds", "minutes", "hours", and "days" according to the value.

_Note_: For this question, it is much more convenient to use tuples.

```ocaml
let approximately_bonus t = "Approximately …"
```


For each question, indicate the type and value or error of the following OCaml expressions:

**Note:** If you believe that the correct option is `A` then you should answer as follows: `let answer = A`.

1. `let r = let x = 7 in 6 * x`

    A) int, 42<br />
    B) int, 6 * x<br />
    C) int, 7<br />
    D) syntax error<br />
    E) Unbound value x<br />
    F) This expression has type string but an expression was expected of type int<br /><br />

2. `let a = (r - 6) / 6 - 6`

    A) int, 1<br />
    B) int, 0<br />
    C) int, -6<br />
    D) This expression has type float but an expression was expected of type int<br />
    E) Exception: Division_by_zero.<br />
    F) Unbound value r<br /><br />

3. `let o = r * r - x * x - 51`

    A) int, -51<br />
    B) int, 0<br />
    C) int, r<br />
    D) Unbound value r<br />
    E) Unbound value x<br />
    F) Syntax error: '(' expected<br /><br />

4. `let u = let x = 9 in if (x<9) then 9 / (x-x) else (x+x) / 9`

    A) int, 9<br />
    B) int, 2<br />
    C) int, 1<br />
    D) This expression has type unit but an expression was expected of type int because it is in the result of a conditional with no else branch<br />
    E) syntax error<br />
    F) Exception: Division_by_zero.<br /><br />

5. `let  x = let a = 10 in if a>7 ||  b / (a - a) then "hello" else "how are you"`

    A) string, "hello"<br />
    B) string, "how are youhello"<br />
    C) string, "how are you"<br />
    D) Unbound value b<br />
    E) This expression has type int but an expression was expected of type string<br />
    F) This expression has type string but an expression was expected of type int<br /><br />

6. `let  x = let a = 10 and b = 7 in if a<7 ||  b / (a - a) > 0 then "hello" else "how are you"`

    A) string, "hello"<br />
    B) string, "how are you"<br />
    C) int, 7<br />
    D) Exception: Division_by_zero.<br />
    E) This expression has type int but an expression was expected of type bool<br />
    F) Unbound value b<br /><br />

7. `let  x = let a = 10 and b = 7 in if a>7 ||  b / (a - a) > 0 then "hello" else "how are you"`

    A) string, "hello"<br />
    B) string, "how are you"<br />
    C) int, 7<br />
    D) Exception: Division_by_zero.<br />
    E) This expression has type int but an expression was expected of type bool<br />
    F) Unbound value b<br /><br />

8. `let x = let a = 10 in if a < 7 && let b = 1 in b / (a - a) then "hello" else "how are you"`

    A) string, "hello"<br />
    B) string, "how are you"<br />
    C) bool, true<br />
    D) Unbound value b<br />
    E) This expression has type int but an expression was expected of type bool<br />
    F) syntax error<br /><br />

9. `let x = let a,b = 10,7 in if a < 7 && a / (b - b) then "hello" else "how are you"`

    A) string, "hello"<br />
    B) string, "how are you"<br />
    C) bool, true<br />
    D) Exception: Division_by_zero.<br />
    E) syntax error<br />
    F) This expression has type int but an expression was expected of type bool<br /><br />

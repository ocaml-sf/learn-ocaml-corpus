For each of the following items, assign which option is the correct one:

**Note:** If you believe that the correct option is `A` then you should answer as follows: `let answer = A`.

1. `let f1 x g z = if x then g x else g z`

    A) bool -> (bool -> 'a) -> bool -> 'a<br />
    B) bool -> (bool -> 'a) -> bool -> (bool * bool)<br />
    C) int -> (string -> unit) -> int -> unit<br />
    D) syntax error<br />
    E) This expression has type bool but an expression was expected of type unit<br />
    F) Unbound value g<br /><br />

2. `let f2 f g x = if f x then g x`

    A) ('a -> int) -> ('a -> unit) -> 'a -> int<br />
    B) ('a -> bool) -> ('a -> unit) -> 'a -> unit<br />
    C) int -> (int -> bool) -> float -> unit<br />
    D) This expression has type int but an expression was expected of type unit because it is in the result of a conditional with no else branch<br />
    E) syntax error<br />
    F) Unbound value f2<br /><br />

3. `let f3 g x y = if g x then g y`

    A) ('a -> bool) -> 'a -> 'a -> bool<br />
    B) bool -> int -> int -> bool<br />
    C) (int -> bool) -> int -> float -> bool<br />
    D) This expression has type bool but an expression was expected of type unit because it is in the result of a conditional with no else branch<br />
    E) Stack overflow during evaluation<br />
    F) Segmentation fault<br /><br />

4. `let rec f4 x y = f4 y ([]::x)`

    A) 'a list list -> 'a list list -> 'b<br />
    B) 'a array array -> 'a array array -> 'b<br />
    C) 'a list -> 'a list -> 'b<br />
    D) Stack overflow during evaluation<br />
    E) Exception: Invalid_argument<br />
    F) Exception: Not_Found<br /><br />

5. `let f5 g f x = if x<0 then f (g x) else g (f x)`

    A) ('a -> 'a) -> ('a -> 'a) -> 'a -> 'a<br />
    B) (float -> int) -> (float -> int) -> float -> int<br />
    C) (int -> int) -> (int -> int) -> int -> int<br />
    D) This expression has type bool but an expression was expected of type unit<br />
    E) This expression has type int but an expression was expected of type unit because it is in the result of a conditional with no else branch<br />
    F) Unbound value x<br /><br />

6. `let f6 g f x y1 = if x<y then  f (g x) else g (f y)`

    A) ('a -> 'a) -> ('a -> 'a) -> 'a -> 'a -> 'a<br />
    B) (int -> int) -> (int -> int) -> int -> int -> int<br />
    C) float -> float -> bool -> bool -> float<br />
    D) This expression has type float but an expression was expected of type int<br />
    E) It is applied to too many arguments; maybe you forgot a `;`<br />
    F) Unbound value y<br /><br />

7. `let f7 x y = if x > 0 || y < 0. then x else y`

    A) int -> int -> int<br />
    B) float -> float -> float<br />
    C) int -> float -> float<br />
    D) This expression has type float but an expression was expected of type int<br />
    E) This expression has type int but an expression was expected of type float<br />
    F) Stack overflow during evaluation<br /><br />
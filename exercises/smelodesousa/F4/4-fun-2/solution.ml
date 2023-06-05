type ('a, 'b, 'c) q1 = int list

let q2 = A

let q3 = D

type ('a, 'b, 'c) q4 = ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c

type ('a, 'b, 'c) q5 = int list

let q6 = D

type ('a, 'b, 'c) q7 = _weak1 -> _weak2 -> _weak1

type ('a, 'b, 'c) q8 = ((((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> (((int -> int) -> int -> int) -> (int -> int) -> int -> int) -> ((int -> int) -> int -> int) -> (int -> int) -> int -> int
type ('a, 'b, 'c) q1 = int * float

type ('a, 'b, 'c) q2 = int -> (int -> 'a) -> int -> 'a

type ('a, 'b, 'c) q3 = 'a -> ('b -> 'c) -> 'b -> 'c

type ('a, 'b, 'c) q4 = (int -> 'a) -> (int -> int) -> int -> 'a

type ('a, 'b, 'c) q5 = ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

type ('a, 'b, 'c) q6 = 'a -> ('a -> 'b) -> ('b -> 'c) -> 'c

type ('a, 'b, 'c) q7 = int list ref

type ('a, 'b, 'c) q8 = _weak1 list ref

type ('a, 'b, 'c) q9 = (int -> int) list

type ('a, 'b, 'c) q10 = int

type ('a, 'b, 'c) q11 = ('a -> 'b) -> ('b list -> 'c) -> 'a list -> 'c
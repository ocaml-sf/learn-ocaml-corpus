Consider the following piece of code:

```ocaml
let rec f x y =
    match x with
    | 0 -> y
    | 2 -> Printf.printf "Case 2\n"; f y 2
    | n -> Printf.printf "Case n\n"; f y 0
```

1. Does the function always terminate?

    A) True<br />
    B) False<br /><br />

2. The function is properly typified with which of the types?

    A) `int -> int -> int`<br />
    B) `int -> int -> unit`<br />
    C) `unit -> unit -> unit`<br />
    D) `ERROR`<br /><br />

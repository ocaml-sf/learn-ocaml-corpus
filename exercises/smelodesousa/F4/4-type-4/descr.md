Consider the following piece of code:

```ocaml
let rec f x =
    match x with
       | 0 -> 0
       | 1 -> f 0
       | 2 -> f (x+1)
       | 3 -> (f 1)+2
       | n -> n+1
```

1. Does the function always terminate?

    A) True<br />
    B) False<br /><br />

2. The function is properly typified with which of the types?

    A) `int`<br />
    B) `int -> int`<br />
    C) `ERROR`<br />
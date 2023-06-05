Consider the following code:

```ocaml
let rec f x =
  match x with
  | 0 -> 1
  | 1 -> 1
  | n -> f(n-1) + f(n-2)
```

1. Does the function always terminate?

    A) True<br />	
    B) False<br /><br />
   
2. What is the type of the function?

    A) `int -> int`<br />	
    B) `int -> int -> int`<br />	
    C) `int`<br />	
    D) `ERROR`<br />

**Note:** If you believe that the correct option is `A` then you should answer as follows: `let answer = A`.

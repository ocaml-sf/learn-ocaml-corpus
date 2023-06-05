Provide the type of the following expressions:

1. `let rec ones = 1::ones`

2. Assuming the previous declaration, what returns `List.length ones`?

    A) The list is endless, so it runs until it reaches _stack overflow_.<br />
    B) The list is endless, so it runs endlessly.<br />
    C) Assuming the list is infinite, it returns an arbitrary value.<br />
    D) Assuming the list is infinite, the execution uses up all memory until it causes a `BSOD`.<br /><br />

3. Assuming the previous declaration, what returns `List.hd ones`?

    A) The function returns `Failure "hd"`.<br />
    B) The list is infinite, so it runs endlessly.<br />
    C) Returns an endless list of 1s except the first.<br />
    D) 1.<br /><br />

4. `fun x -> fun y -> fun z -> x z (y z)`

5. `let x = (List.tl [3]) in 1::x`

6. Consider the following declarations: `let l = ref []` and `let nl = 1::!l`. What does the following declaration do: `let nll = 'c':: !l`?

    A) `nll` becomes equal to `['c'; 1]`.<br />
    B) `nll` becomes equal to `[1; 'c']`.<br />
    C) `nll` becomes equal to `[99; 1]`, where `99` is the ASCII code of `c`.<br />
    D) Returns a type error.<br /><br />

7. `let k = fun x y -> x in let i = fun x -> x in k k i`

8. Consider the following code:

    ```ocaml
    let b = true
    let f0 = fun x -> x+1
    let f = fun x -> if b then f0 else fun y -> x y
    let f = fun x -> if b then f else fun y -> x y
    let f = fun x -> if b then f else fun y -> x y
    let f = fun x -> if b then f else fun y -> x y
    ```

    What is the type of the last `f`?


**Note:** If necessary, use the type `'_weak1`. It should be used as follows:<br />
  `type answer = _weak1`, which means, without the apostrophe.
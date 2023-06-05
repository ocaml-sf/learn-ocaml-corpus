**Question :**  For each of the OCaml phrases below, predict the calculated
result value by OCaml for that phrase and indicate this value on the left
instead of the corresponding -1. Then, verify your answers using automatic
notation (click the "Grade!" button above).

```ocaml

let phrase0 =
    let x = 3
    in x + 1

let phrase1 =
  let x = 3 in
  let y = x + 1
  in x + y

let phrase2 =
  let x = 2 in
  let x = x + x
  in x + x

(* notice the "and" syntax and guess what it means *)
let phrase3 =
  let x = 2 in
  let x = 3 and y = x + 1
  in x + y
```

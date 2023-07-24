(* Question 1. *)
let interval10 x = false

(* Question 2. *)
let value1 = true && false

let value2 = true || false

let value3 = not (true || true) && true

(* Question 3.*)
let simplify1 x y = (x || y) || x

let simplify2 x y = (x > 5 && x >= 7)

let simplify3 x y z = x = y && y = z && x = z

let simplify4 x y = x > 7 || (x <= 7 && y > 2)

let simplify5 x = (((x = true) = false) = false) = true

(*Question 4. *)

let leap x = false

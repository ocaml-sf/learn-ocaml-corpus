(* Question 1. *)
let interval10 x = x >= 0 && x <= 10  

(* Question 2. *)
let value1 = false

let value2 = true

let value3 = false

(* Question 3.*)
let simplify1 x y = x || y

let simplify2 x y = x >= 7

let simplify3 x y z = x = y && y = z

let simplify4 x y = x > 7 || y > 2

let simplify5 x = x

(*Question 4. *)

let leap x = (x mod 4 = 0) && (x mod 100 <> 0 || x mod 400 = 0)

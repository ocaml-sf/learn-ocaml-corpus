(* Question 1. *)
let intervalle10 x = x >= 0 && x <= 10  

(* Question 2. *)
let valeur1 = false

let valeur2 = true

let valeur3 = false

(* Question 3.*)
let simplifier1 x y = x || y

let simplifier2 x y = x >= 7

let simplifier3 x y z = x = y && y = z

let simplifier4 x y = x > 7 || y > 2

let simplifier5 x = x

(*Question 4. *)

let bissextile x = (x mod 4 = 0) && (x mod 100 <> 0 || x mod 400 = 0)

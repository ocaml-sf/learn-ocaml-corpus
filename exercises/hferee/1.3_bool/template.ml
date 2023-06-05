(* Question 1. *)
let intervalle10 x = false

(* Question 2. *)
let valeur1 = true && false

let valeur2 = true || false

let valeur3 = not (true || true) && true

(* Question 3.*)
let simplifier1 x y = (x || y) || x

let simplifier2 x y = (x > 5 && x >= 7)

let simplifier3 x y z = x = y && y = z && x = z

let simplifier4 x y = x > 7 || (x <= 7 && y > 2)

let simplifier5 x = (((x = true) = false) = false) = true

(*Question 4. *)

let bissextile x = false

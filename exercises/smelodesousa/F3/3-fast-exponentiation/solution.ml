(* 1 *)
let rec fast_exp x = function
  | 0 -> 1 
  | 1 -> x
  | n -> let a = fast_exp x (n / 2) in
      a * a * (if n mod 2 = 0 then 1 else x)

(* 2 *)
let answer = D
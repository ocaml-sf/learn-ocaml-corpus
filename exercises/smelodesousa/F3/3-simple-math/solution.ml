let square x = x *. x

let distance p1 p2 =
  let x1, y1 = p1 and x2, y2 = p2 in
  sqrt (square (x2 -. x1) +. square (y2 -. y1))

let area r = pi *. r *. r
(* pi *. square r *)

let sin2x x = 2.0 *. tan x /. (1.0 +. square (tan x))

let cartesian_of_polar {r; angle} =
  { x = r *. cos(angle); y = r *. sin(angle); }


let middle_cart a b = { x = (a .x +. b.x) /. 2.; y = (a.y +. b.y) /. 2.; }

let ensure_cart = function
  | Polar p -> cartesian_of_polar p
  | Cartesian foo -> foo

let middle a b = Cartesian (middle_cart (ensure_cart a) (ensure_cart b))

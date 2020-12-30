let move { x; y; z } { dx; dy; dz } =
  { x = x +. dx; y = y +. dy; z = z +. dz }

let next { position; velocity } =
  { position = move position velocity; velocity }

let will_collide_soon p1 p2 =
  let p1 = next p1 in
  let p2 = next p2 in
  ((p1.position.x -. p2.position.x) ** 2. +.
   (p1.position.y -. p2.position.y) ** 2. +.
   (p1.position.z -. p2.position.z) ** 2.) |> sqrt < 2.

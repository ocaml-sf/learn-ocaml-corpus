let one_triangle = draw (triangle 1.)

let two_triangles =
  draw (fun p -> triangle 0.5 (move_by 0.5 0.0 (triangle 0.5 p)))

let rec draw_sierpinsky size n = if n = 0 then triangle size else
    let size' = (size /. 2.) in
    let d = draw_sierpinsky size' (n-1) in
    fun p -> p |> d
             |> move_by size' 0. |> d
             |> move_by (- 0.5 *. size') (size *. (sqrt 3.) /. 4.)
             |> d |> move_by (-0.5 *. size') (-.(size *. (sqrt 3.) /. 4.))


let sierpinsky n = draw (draw_sierpinsky 1. n)

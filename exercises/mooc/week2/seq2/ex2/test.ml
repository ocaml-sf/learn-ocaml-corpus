open Report
open Test_lib

let print_float ppf = Format.fprintf ppf "%.3f" ;;
(* #install_printer print_float ;; *)

let dist p1 p2 =
  sqrt ((p1.x -. p2.x) ** 2. +. (p1.y -. p2.y) ** 2. +. (p1.z -. p2.z) ** 2.)

let sample_point () =
  let dim () = Random.float 5. -. 2.5 in
  { x = dim () ; y = dim () ; z = dim () }

let sample_dpoint () =
  let comp () = Random.float 2. -. 1. in
  { dx = comp () ; dy = comp () ; dz = comp () }

let sample_physical_object () =
  let position = sample_point () in
  let velocity = sample_dpoint () in
  { position ; velocity }

let exercise_1 =
  Section ([ Text "Exercise 1: " ; Code "move" ],
           test_function_2_against_solution
             ~test: (test_eq_ok (fun o1 o2 -> dist o1 o2 < 0.01))
             [%ty: point -> dpoint -> point] "move"
             [])

let exercise_2 =
  Section ([ Text "Exercise 2: " ; Code "next" ],
           test_function_1_against_solution
             ~test: (test_eq_ok (fun o1 o2 ->
                 dist o1.position o2.position < 0.01
                 && o1.velocity = o2.velocity))
             [%ty: physical_object -> physical_object] "next"
             [])

let sample_spheres () =
  let rec sample_until pred =
    let o1 = sample_physical_object () in
    let o2 = sample_physical_object () in
    if pred (dist o1.position o2.position) then (o1, o2) else sample_until pred in
  if Random.bool () then
    sample_until (fun x -> x < 1.9)
  else
    sample_until (fun x -> x > 2.1)

let exercise_3 =
  Section ([ Text "Exercise 3: " ; Code "will_collide_soon" ],
           test_function_2_against_solution
             ~sampler: sample_spheres
             [%ty: physical_object -> physical_object -> bool] "will_collide_soon"
             [])


let () =
  set_result
    (ast_sanity_check code_ast @@ fun () ->
     [ exercise_1 ; exercise_2 ; exercise_3 ])

open Report
open Test_lib

let sample_point2d () =
  Random.int 100 - 50,
  Random.int 100 - 50

let rec sample_tetragon () =
  sample_point2d (),
  sample_point2d (),
  sample_point2d (),
  sample_point2d ()

let exercise_1 =
  Section ([ Text "Exercise 1:" ; Code "pairwise_distinct" ],
           test_function_1_against_solution
             [%ty: tetragon -> bool] "pairwise_distinct"
             [ (0,0), (0,0), (0,0), (0,0) ;
               (0,1), (2,3), (4,5), (6,7) ;
               (0,0), (-12,-12), (33,33), (-12,-12) ;
               (0,1), (0,2), (0,1), (0,0) ])

let rec sample_tetragon () =
  let res =
    sample_point2d (),
    sample_point2d (),
    sample_point2d (),
    sample_point2d () in
  if Solution.pairwise_distinct res then
    res
  else
    sample_tetragon ()

let exercise_2 =
  Section ([ Text "Exercise 2:" ; Code "wellformed" ],
           test_function_1_against_solution
             [%ty: tetragon -> bool] "wellformed"
             [ (0,23), (-3,-5), (8,13), (12,0) ;
               (0,1), (2,3), (4,5), (6,7) ;
               (0,0), (-12,-12), (33,33), (32,-12) ])

let exercise_3 =
  Section ([ Text "Exercise 3:" ; Code "rotate_point" ],
           test_function_1_against_solution
             [%ty: point2d -> point2d] "rotate_point"
             [ (0,23) ; (-3,-5) ; (8,13) ; (12,0) ; (0,0) ; (2,3) ])

let sample_wellformed_tetragon () =
  let vert = Random.int 100 - 50 in
  let hor = Random.int 100 - 50 in
  let l = vert - Random.int 20 - 1 in
  let r = vert + Random.int 20 in
  let lup = (l, hor + Random.int 20 + 1) in
  let rup = (r, hor + Random.int 20 + 1) in
  let llp = (l - Random.int 5 - 1, hor - Random.int 20 - 1) in
  let rlp = (r + Random.int 5 + 1, hor - Random.int 20 - 1) in
  (lup, rup, llp, rlp)

let sample_tetragon () =
  let (a,b,c,d) = sample_wellformed_tetragon () in
  let (a,b,c,d) = if Random.bool () then (c, d, a, b) else (a,b,c,d) in
  let (a,b,c,d) = if Random.bool () then (b, a, d, c) else (a,b,c,d) in
  (a,b,c,d)

let exercise_4 =
  Section ([ Text "Exercise 4:" ; Code "reorder" ],
           test_function_1_against_solution
             [%ty: tetragon -> tetragon] "reorder"
             [ (* auto gen *) ])

let sample_tetragon = sample_wellformed_tetragon

let exercise_5 =
  Section ([ Text "Exercise 5:" ; Code "rotate_tetragon" ],
           test_function_1_against_solution
             [%ty: tetragon -> tetragon] "rotate_tetragon"
             [ (* auto gen *) ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ; exercise_5 ]

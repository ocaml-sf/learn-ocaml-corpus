open Report
open Test_lib

let rec sample_bt ~balanced sample () =
  let rec rew = function
    | Empty -> Empty
    | Node (l, _, r) -> Node (rew l, sample (), rew r) in
  let rec sample_rec level =
    if Random.int level = 0 then  Empty
    else if balanced then
      let s = sample_rec (level - 1) in Node (s, sample (), rew s)
    else Node (sample_rec (level - 1), sample (), sample_rec (level - 1)) in
  match sample_rec 6 with
  | Empty -> sample_bt ~balanced sample ()
  | res when Solution.balanced res = balanced -> res
  | _ -> sample_bt ~balanced sample ()

let sample_bt sample =
  sample_alternatively
    [ (fun () -> sample_bt ~balanced: true sample ()) ;
      (fun () -> sample_bt ~balanced: false sample ()) ]

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "height" ],
           test_function_1_against_solution ~gen: 5
             [%ty: int bt -> int] "height"
             [ ] @
           test_function_1_against_solution ~gen: 5
             [%ty: char bt -> int] "height"
             [ Empty ])

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "balanced" ],
           test_function_1_against_solution ~gen: 5
             [%ty: int bt -> bool] "balanced"
             [ ] @
           test_function_1_against_solution ~gen: 5
             [%ty: char bt -> bool] "balanced"
             [ Empty ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ]

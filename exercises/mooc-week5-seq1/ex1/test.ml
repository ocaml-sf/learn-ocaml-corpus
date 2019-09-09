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
  | res when fst (Solution.balanced res) = balanced -> res
  | _ -> sample_bt ~balanced sample ()

let sample_bt sample =
  sample_alternatively
    [ (fun () -> sample_bt ~balanced: true sample ()) ;
      (fun () -> sample_bt ~balanced: false sample ()) ]

let exercise_1_height =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "height" ],
           test_function_1_against_solution ~gen: 5
             [%ty: int bt -> int * int] "height"
             [] @
           test_function_1_against_solution ~gen: 5
             [%ty: char bt -> int * int] "height"
             [ Empty ])

let exercise_1_balanced =
  Section ([ Text "Exercise 1: " ; Code "balanced" ],
           test_function_1_against_solution ~gen: 5
             [%ty: int bt -> bool * int] "balanced"
             [] @
           test_function_1_against_solution ~gen: 5
             [%ty: char bt -> bool * int] "balanced"
             [ Empty ])

let exercise_2 =
  set_progress "Grading exercise 2." ;
  let test = test_eq @@ fun a b ->  match a, b with
    | Ok _, Error _ | Error _, Ok _ -> false
    | Ok (hgot, got), Ok (hexp, exp) ->
        hgot = hexp && got <= !Solution.max_visited && got >= !Solution.min_visited
    | Error (Unbalanced got), Error (Unbalanced exp) ->
        got <= !Solution.max_visited && got >= !Solution.min_visited
    | Error _, Error _ -> false in
  Section ([ Text "Exercise 2: " ; Code "bal_height" ],
           test_function_1_against_solution ~test ~gen: 5
             [%ty: int bt -> int * int] "bal_height"
             [] @
           test_function_1_against_solution ~test ~gen: 5
             [%ty: char bt -> int * int] "bal_height"
             [ Empty ])

let exercise_3 =
  set_progress "Grading exercise 3." ;
  let test = test_eq_ok @@ fun (rgot, got) (rexp, exp) ->
    rexp = rgot && got <= !Solution.max_visited && got >= !Solution.min_visited in
  Section ([ Text "Exercise 3: " ; Code "balanced_fast" ],
           test_function_1_against_solution ~test ~gen: 5
             [%ty: int bt -> bool * int] "balanced_fast"
             [] @
           test_function_1_against_solution ~test ~gen: 5
             [%ty: char bt -> bool * int] "balanced_fast"
             [ Empty ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1_height ;
    exercise_1_balanced ;
    exercise_2 ;
    exercise_3 ]

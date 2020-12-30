open Report
open Test_lib

let rec sample_clist sample () =
  let rec sample_rec level =
    match Random.int level with
    | 0 -> CEmpty
    | 1 -> CSingle (sample ())
    | _ -> CApp (sample_rec (level - 1), sample_rec (level - 1)) in
  match sample_rec 5 with
  | CEmpty -> sample_clist sample ()
  | res -> res

let exercise_1 =
  set_progress "Grading exercise 1." ;
  Section ([ Text "Exercise 1: " ; Code "to_list" ],
           test_function_1_against_solution ~gen: 5
             [%ty: int clist -> int list] "to_list"
             [] @
           test_function_1_against_solution ~gen: 5
             [%ty: char clist -> char list] "to_list"
             [ CEmpty ])

let sample_list s = sample_list ~min_size: 1 s

let test_clist x y =
  test_eq_ok (fun l1 l2 -> Solution.to_list l1 = Solution.to_list l2) x y

let test_clist_option x y =
  test_eq_ok (fun l1 l2 -> match l1, l2 with Some l1, Some l2 -> Solution.to_list l1 = Solution.to_list l2 | _ -> l1 = l2) x y

let exercise_2 =
  set_progress "Grading exercise 2." ;
  Section ([ Text "Exercise 2: " ; Code "of_list" ],
           test_function_1_against_solution ~gen: 5
             ~test:test_clist
             [%ty: int list -> int clist] "of_list"
             [] @
           test_function_1_against_solution ~gen: 5
             ~test:test_clist
             [%ty: char list -> char clist] "of_list"
             [ [] ])

let exercise_3 =
  set_progress "Grading exercise 3." ;
  Section ([ Text "Exercise 3: " ; Code "append" ],
           test_function_2_against_solution ~gen: 5
             ~test:test_clist
             [%ty: int clist -> int clist -> int clist] "append"
             [ CEmpty, sample_clist sample_int () ] @
           test_function_2_against_solution ~gen: 5
             ~test:test_clist
             [%ty: char clist -> char clist -> char clist] "append"
             [ sample_clist sample_char (), CEmpty ])

let exercise_4 =
  set_progress "Grading exercise 4." ;
  Section ([ Text "Exercise 4: " ; Code "hd" ],
           test_function_1_against_solution ~gen: 5
             [%ty: int clist -> int option] "hd"
             [] @
           test_function_1_against_solution ~gen: 5
             [%ty: char clist -> char option] "hd"
             [ CEmpty ])

let exercise_5 =
  set_progress "Grading exercise 5." ;
  Section ([ Text "Exercise 5: " ; Code "tl" ],
           test_function_1_against_solution ~gen: 5
             ~test:test_clist_option
             [%ty: int clist -> int clist option] "tl"
             [] @
           test_function_1_against_solution ~gen: 5
             ~test:test_clist_option
             [%ty: char clist -> char clist option] "tl"
             [ CEmpty ])

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ exercise_1 ; exercise_2 ; exercise_3 ; exercise_4 ; exercise_5 ]


open Test_lib
open Report

let test_1_with_solution =
  set_progress "Grading exercise 1";
  Section
    ( [ Text "Exercise 1: "; Code "dragon_size" ],
      test_function_1_against_solution [%ty: int -> int] "dragon_size"
        ~sampler:(fun () -> Random.int 200 - 50)
        ~gen:17 [ -1; 0; 1 ] )

let test_2_with_solution =
  set_progress "Grading exercise 2";
  Section
    ( [ Text "Exercise 2: "; Code "dragon" ],
      test_function_1_against_solution [%ty: int -> bool list] "dragon"
        ~sampler:(fun () -> Random.int 25 - 5)
        ~gen:17 [ -1; 0; 1; 20 ] )

let test_3_with_solution =
  set_progress "Grading exercise 3";
  Section
    ( [ Text "Exercise 3: "; Code "dragon_bit" ],
      test_function_1_against_solution [%ty: int -> bool] "dragon_bit"
        ~sampler:(fun () -> Random.int 1 - 5)
        ~gen:0 [ -1; 0; 1; 16; 2 ] )

let () =
  set_result @@ ast_sanity_check code_ast
  @@ fun () ->
  [ test_1_with_solution; test_2_with_solution; test_3_with_solution ]

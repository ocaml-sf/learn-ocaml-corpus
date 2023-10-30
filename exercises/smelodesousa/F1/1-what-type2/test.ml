open Test_lib
open Report

let ex1 =
  set_progress "Grading exercise 1";
  Section
    ( [ Text "Exercise 1: "; Code "solution" ],
      test_variable_against_solution [%ty: choice] "q1" )

let ex2 =
  set_progress "Grading exercise 2";
  Section
    ( [ Text "Exercise 2: "; Code "solution" ],
      test_variable_against_solution [%ty: choice] "q2" )

let ex3 =
  set_progress "Grading exercise 3";
  Section
    ( [ Text "Exercise 3: "; Code "solution" ],
      test_variable_against_solution [%ty: choice] "q3" )

let ex4 =
  set_progress "Grading exercise 4";
  Section
    ( [ Text "Exercise 4: "; Code "solution" ],
      test_variable_against_solution [%ty: choice] "q4" )

let () =
  set_result @@ ast_sanity_check code_ast @@ fun () -> [ ex1; ex2; ex3; ex4 ]

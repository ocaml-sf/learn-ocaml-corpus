open Test_lib
open Report

let ex1 =
  set_progress "Grading exercise 1" ;
  Section ([ Text "Exercise 1: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "p1")

let ex2 =
  set_progress "Grading exercise 2" ;
  Section ([ Text "Exercise 2: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "p2")

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ ex1; ex2]

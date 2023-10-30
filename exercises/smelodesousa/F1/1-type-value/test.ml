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

let ex3 =
  set_progress "Grading exercise 3" ;
  Section ([ Text "Exercise 3: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "p3")

let ex4 =
  set_progress "Grading exercise 4" ;
  Section ([ Text "Exercise 4: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "p4")
            
let ex5 =
  set_progress "Grading exercise 5" ;
  Section ([ Text "Exercise 5: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "p5")

let ex6 =
  set_progress "Grading exercise 6" ;
  Section ([ Text "Exercise 6: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "p6")

let ex7 =
  set_progress "Grading exercise 7" ;
  Section ([ Text "Exercise 7: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: choice ] 
             "p7")

let ex8 =
  set_progress "Grading exercise 8" ;
  Section ([ Text "Exercise 8: " ; Code "solution" ],
            test_variable_against_solution
              [%ty: choice ] 
              "p8")

let ex9 =
  set_progress "Grading exercise 9" ;
  Section ([ Text "Exercise 9: " ; Code "solution" ],
            test_variable_against_solution
              [%ty: choice ] 
              "p9")

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ ex1; ex2; ex3; ex4; ex5; ex6; ex7; ex8; ex9]

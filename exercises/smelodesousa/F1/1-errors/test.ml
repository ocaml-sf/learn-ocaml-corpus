open Test_lib
open Report

let ex1 =
  set_progress "Correcting question 1" ;
  Section ([ Text "quarter_pi: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: float ] 
             "quarter_pi")

let ex2 =
  set_progress "Correcting question 2" ;
  Section ([ Text "in_order: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: bool ] 
             "in_order")

let ex3 =
  set_progress "Correcting question 3" ;
  Section ([ Text "positive: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: bool ] 
             "positive")

let ex4 =
  set_progress "Correcting question 4" ;
  Section ([ Text "double_positive: " ; Code "solution" ],
            test_variable_against_solution
             [%ty: float ] 
             "double_positive")

let () =
  set_result                @@
  ast_sanity_check code_ast @@ fun () ->
  [ ex1; ex2; ex3; ex4 ]

open Test_lib
open Report

let testAnswer1 = 
  set_progress "Correcting exercise 1" ;
  Section ([ Text "Exercise 1: " ; Code "" ],
           test_variable_against_solution
            [%ty: char list list] 
            "answer1")

let testAnswer2 = 
  set_progress "Correcting exercise 2" ;
  Section ([ Text "Exercise 2: " ; Code "" ],
            test_variable_against_solution
            [%ty: char list list] 
            "answer2")

let testAnswer3 = 
  set_progress "Correcting exercise 3" ;
  Section ([ Text "Exercise 3: " ; Code "" ],
            test_variable_against_solution
            [%ty: char list list] 
            "answer3")


let testInsertions = 
  set_progress "Correcting exercise 4" ;
  Section (
            [ Text "Exercise 4: " ; Code "" ],
            test_function_2_against_solution
              [%ty: char -> char list -> char list list] 
              "insertion"
              ~sampler: (fun () -> (sample_char(), (sample_list ~min_size: 3 ~max_size: 9 sample_char)()))
              ~gen: 7
              []
          )

let () =
  set_result @@
  ast_sanity_check code_ast @@ fun () ->
  [ testAnswer1 ; testAnswer2 ; testAnswer3 ; testInsertions ]
